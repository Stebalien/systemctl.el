;;; systemctl.el --- Systemctl -*- lexical-binding: t -*-

;; Copyright 2020 Steven Allen <steven@stebalien.com>

;; Author: Steven Allen <steven@stebalien.com>
;; URL: https://github.com/Stebalien/systemctl.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: systemd, unix

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; An Emacs package for controlling systemd over dbus.

;;; Requirements:

;; Emacs 27.0.0

;;; Code:
(require 'transient)
(require 'dbus)

(eval-when-compile (require 'cl-lib))

(defgroup systemctl nil
  "Systemd control utilities."
  :version "0.0.1"
  :prefix "systemctl-"
  :group 'tools)

(defcustom systemctl-unit-types '("service" "timer")
  "Systemd unit types to display in completion."
  :version "0.0.1"
  :type '(choice (const :tag "All" nil)
                 (repeat :tag "Unit Types" string)))

(defun systemctl--remove-keyword-params (seq)
  "Remove all keyword/value pairs from SEQ."
  (if (null seq) nil
    (let ((head (car seq))
          (tail (cdr seq)))
      (if (keywordp head) (systemctl--remove-keyword-params (cdr tail))
        (cons head (systemctl--remove-keyword-params tail))))))

(cl-defun systemctl-manage-systemd (method &rest args &key user async &allow-other-keys)
  "Invoke a management METHOD on systemd with the specified ARGS.

Specify USER to manage a user unit, and ASYNC to invoke dbus asynchronously.
If ASYNC is a function, it'll be called when the method completes."
  (setq args (systemctl--remove-keyword-params args))
  (when (and (not noninteractive) (version<= "31.0" emacs-version))
    (setq args (append '(:authorizable t) args)))
  (if async
      (apply #'dbus-call-method-asynchronously
             (if user :session :system)
             "org.freedesktop.systemd1" "/org/freedesktop/systemd1"
             "org.freedesktop.systemd1.Manager" method (and (functionp async) async)
             args)
    (apply #'dbus-call-method
           (if user :session :system)
           "org.freedesktop.systemd1" "/org/freedesktop/systemd1"
           "org.freedesktop.systemd1.Manager" method args)))

(defun systemctl--completion-annotation (unit)
  "Completion annotation function used when prompting for a systemd UNIT."
  (concat
   (propertize " " 'display '(space :align-to center))
   (get-text-property 0 'systemctl--unit-description unit)))

(defun systemctl--completion-group (unit transform)
  "Completion group function used when prompting for a systemd UNIT.

If TRANSFORM is non-nil, returns the unit's basename.
Otherwise, return a group name suitable for the unit."
  (if transform
      (file-name-base unit)
    (concat (directory-file-name (file-name-directory unit))
            ":"
            (file-name-extension unit))))

(defconst systemctl--completion-properties
  `((group-function . ,#'systemctl--completion-group)
    (annotation-function . ,#'systemctl--completion-annotation))
  "Completion properties for `systemctl-read-unit' prompts.")

(defun systemctl--choose-unit (prompt units)
  "PROMPT for a unit from the given list of UNITS."
  (let ((candidates
         (seq-map (lambda (unit)
                    (let ((name (car unit))
                          (user (plist-get (cdr unit) :user))
                          (desc (plist-get (cdr unit) :description)))
                      (cons (propertize
                             (concat (if user "user/" "system/") name)
                             'systemctl--unit-description desc)
                            unit)))
                  units)))
    (alist-get (completing-read prompt (completion-table-with-metadata
                                        candidates
                                        systemctl--completion-properties)
                                nil t)
               candidates
               nil nil #'string=)))

(defun systemctl--parse-filter (filter)
  "Parse FILTER into an alist with the keys `user' `system' and `patterns'."
  (cl-loop with user-only and system-only
           for elt in filter
           if (eq elt 'user)
             do (setq user-only t)
           else if (eq elt 'system)
             do (setq system-only t)
           else
             collect (format "*.%s" elt) into patterns
           finally return
             `((user . ,(or user-only (not system-only)))
               (system . ,(or system-only (not user-only)))
               (patterns . ,patterns))))

(defun systemctl--list-units (user patterns)
  "List all USER or system unit files, filtering by PATTERNS if non-empty."
  (thread-last
    (systemctl-manage-systemd "ListUnitsByPatterns"
                              :user user
                              '(:array) ; all states
                              (cons :array patterns))
    (seq-map (pcase-lambda (`(,unit ,desc . ,_))
               (list unit :user user :description desc)))))

(defun systemctl--list-unit-files (user patterns)
  "List all USER or system unit files, filtering by PATTERNS if non-empty."
      (thread-last
        (systemctl-manage-systemd "ListUnitFilesByPatterns"
                                  :user user
                                  '(:array) ; all states
                                  (cons :array patterns))
        (seq-map 'car)
        (seq-map 'file-name-nondirectory)
        (seq-sort 'string-lessp)
        (delete-consecutive-dups)
        (seq-map (lambda (unit) (list unit :user user)))))

(defun systemctl-read-unit (&optional prompt &rest filter)
  "Prompt for a unit (limited to loaded units).

PROMPT is a string to prompt with.
FILTER limits the units to prompt for. It can contain:
- Any number of symbols/strings limiting the shown unit types (service,
  timer, etc.).
- The symbols `user' and/or `system'. If only one of these is specified,
  only the specifie units (user or system) will be shown. By default,all
  units (both user and system) are shown."
  (let-alist (systemctl--parse-filter filter)
    (cl-destructuring-bind (name &key user &allow-other-keys)
        (systemctl--choose-unit (or prompt "Unit: ")
                                (append (when .user (systemctl--list-units t .patterns))
                                        (when .system (systemctl--list-units nil .patterns))))
      (list name :user user))))


(defun systemctl-read-unit-file (&optional prompt &rest filter)
  "Prompt for a file (all known units).
The user will be asked to fill in any templates.

PROMPT is a string to prompt with.
FILTER limits the units to prompt for. It can contain:
- Any number of symbols/strings limiting the shown unit types (service,
  timer, etc.).
- The symbols `user' and/or `system'. If only one of these is specified,
  only the specifie units (user or system) will be shown. By default,all
  units (both user and system) are shown."
  (let-alist (systemctl--parse-filter filter)
    (let* ((units
            (append (when .user (systemctl--list-unit-files t .patterns))
                    (when .system (systemctl--list-unit-files nil .patterns))))
           (unit (systemctl--choose-unit (or prompt "Unit file: ") units)))
    (when (string-suffix-p "@" (file-name-base (car unit)))
      (with-temp-buffer
        (call-process "systemd-escape" nil t nil
                      "--template"
                      (car unit)
                      (read-string "Instance: "))
        (setcar unit (string-trim (buffer-string)))))
    unit)))

;;;###autoload
(cl-defun systemctl-start (unit &key user)
  "Start a UNIT.

Specify USER to manage a user unit."
  (interactive (apply #'systemctl-read-unit-file "Start Service: " systemctl-unit-types))
  (systemctl-manage-systemd "StartUnit" unit "replace" :user user :async t))

;;;###autoload
(cl-defun systemctl-stop (unit &key user)
  "Start a systemd UNIT.

Specify USER to restart a user unit."
  (interactive (apply #'systemctl-read-unit "Stop Service: " systemctl-unit-types))
  (systemctl-manage-systemd "StopUnit" unit "replace" :user user :async t))

;;;###autoload
(cl-defun systemctl-restart (unit &key user try)
  "Restart the systemd UNIT.

Specify USER to restart a user unit.
Specify TRY to try to restart the unit, if and only if it's already running."
  (interactive (apply #'systemctl-read-unit "Restart Service: " systemctl-unit-types))
  (systemctl-manage-systemd
   (if try "TryRestartUnit" "RestartUnit")
   :user user
   :async t
   unit "replace"))

;;;###autoload
(cl-defun systemctl-reload (unit &key user or-restart)
  "Reload the systemd UNIT.

Specify USER to reload a user unit.
Specify OR-RESTART to restart the unit if it cannot be reloaded."
  (interactive (apply #'systemctl-read-unit "Reload Service: " systemctl-unit-types))
  (systemctl-manage-systemd
   (pcase or-restart
     ('t "ReloadOrRestartUnit")
     ('try "ReloadOrTryRestartUnit")
     ('nil "ReloadUnit")
     (_ (error "`or-restart' must either be `t', `'try', or `nil'")))
   :user user
   :async t
   unit "replace"))

;;;###autoload
(cl-defun systemctl-daemon-reload (&key user)
  "Reload the systemd configuration.

Specify USER to reload the configuration of the user daemon."
  (interactive (list :user
                 (pcase (read-answer
                         "Reload the [s]ystem or [u]ser daemon? "
                         '(("system" ?s "reload the system daemon")
                           ("user" ?u "reload the user daemon")
                           ("quit" ?q "abort")))
                   ("system" nil)
                   ("user" t)
                   (_ (keyboard-quit)))))
  (systemctl-manage-systemd "Reload" :user user :async t))

(cl-defun systemctl-manage-logind (method &rest args &key async &allow-other-keys)
  "Invoke a management METHOD on logind with the specified ARGS.

If ASYNC is non-nil, invoke asynchronously."
  (setq args (systemctl--remove-keyword-params args))
  (when (and (not noninteractive) (version<= "31.0" emacs-version))
    (setq args (append '(:authorizable t) args)))
  (if async
      (apply #'dbus-call-method-asynchronously
             :system "org.freedesktop.login1"
             "/org/freedesktop/login1"
             "org.freedesktop.login1.Manager" method nil args)
    (apply #'dbus-call-method
           :system "org.freedesktop.login1"
           "/org/freedesktop/login1"
           "org.freedesktop.login1.Manager" method  args)))

(defun systemctl--logind-property (name)
  "Get the logind property named NAME."
  (dbus-get-property
   :system "org.freedesktop.login1"
   "/org/freedesktop/login1"
   "org.freedesktop.login1.Manager" name))

;;; Lock/Unlock

(defun systemctl--lock-unlock-common (action &optional session)
  "Lock or Unlock (ACTION) the specified SESSION."
  (cond
   ((null session) (dbus-call-method-asynchronously
                    :system "org.freedesktop.login1"
                    "/org/freedesktop/login1/session/auto"
                    "org.freedesktop.login1.Session" action nil))
   ((eq session t) (systemctl-manage-logind (concat action "Sessions")))
   ((stringp session) (systemctl-manage-logind (concat action "Session") session))
   (t (error "Invalid `session' argument"))))

;;;###autoload
(defun systemctl-lock (&optional session)
  "Lock the current or specified SESSION."
  (interactive)
  (systemctl--lock-unlock-common "Lock" session))

;;;###autoload
(defun systemctl-unlock (&optional session)
  "Lock the current or specified SESSION."
  (interactive)
  (systemctl--lock-unlock-common "Unlock" session))

;;; Suspend

(defun systemctl--can-suspend-p ()
  "Check if system can suspend."
  (string= "yes" (systemctl-manage-logind "CanSuspend")))

;;;###autoload(autoload 'systemctl-suspend "systemctl" nil t)
(transient-define-suffix systemctl-suspend ()
  "Suspend the system."
  :description "Suspend"
  :inapt-if-not 'systemctl--can-suspend-p
  (interactive)
  (systemctl-manage-logind "Suspend" t))

;;; Hibernate

(defun systemctl--can-hibernate-p ()
  "Check if system can hibernate."
  (string= "yes" (systemctl-manage-logind "CanHibernate")))

;;;###autoload(autoload 'systemctl-hibernate "systemctl" nil t)
(transient-define-suffix systemctl-hibernate ()
  "Hibernate the system."
  :description "Hibernate"
  :inapt-if-not 'systemctl--can-hibernate-p
  (interactive)
  (systemctl-manage-logind "Hibernate" t))

;;; Hybrid-sleep

(defun systemctl--can-hybrid-sleep-p ()
  "Check if system can hybrid-sleep."
  (string= "yes" (systemctl-manage-logind "CanHybridSleep")))

;;;###autoload(autoload 'systemctl-hybrid-sleep "systemctl" nil t)
(transient-define-suffix systemctl-hybrid-sleep ()
  "Hybrid suspend/sleep (suspend and hibernate at the same time)."
  :description "Hybrid sleep (suspend to both)"
  :inapt-if-not 'systemctl--can-hybrid-sleep-p
  (interactive)
  (systemctl-manage-logind "HybridSleep" t))

;;; Suspend then hibernate

(defun systemctl--can-suspend-then-hibernate-p ()
  "Check if system can suspend-then-hibernate."
  (string= "yes" (systemctl-manage-logind "CanSuspendThenHibernate")))

;;;###autoload(autoload 'systemctl-suspend-then-hibernate "systemctl" nil t)
(transient-define-suffix systemctl-suspend-then-hibernate ()
  "Suspend now then hibernate once the system reaches a critical battery level."
  :description "Suspend Then Hibernate"
  :inapt-if-not 'systemctl--can-suspend-then-hibernate-p
  (interactive)
  (systemctl-manage-logind "SuspendThenHibernate" t))

;;; Sleep

(defun systemctl--can-sleep-p ()
  "Check if system can sleep (using the default method)."
  (string= "yes" (systemctl-manage-logind "CanSleep")))

(defconst systemctl--sleep-actions
  '(("suspend-then-hibernate" . systemctl--can-suspend-then-hibernate-p)
    ("hybrid-sleep" . systemctl--can-hybrid-sleep-p)
    ("suspend" . systemctl--can-suspend-p)
    ("hibernate" . systemctl--can-hibernate-p)))

(defun systemctl--get-sleep-operation ()
  "Get the default sleep operation."
  (let ((ops (systemctl--logind-property "SleepOperation")))
    (cl-loop for (action . test) in systemctl--sleep-actions
             when (member action ops)
             when (funcall test)
             return action)))

;;;###autoload(autoload 'systemctl-sleep "systemctl" nil t)
(transient-define-suffix systemctl-sleep ()
  "Put the system to sleep via the default suspend mechanism."
  :description (lambda () (format "Sleep (%s)" (or (systemctl--get-sleep-operation) "none")))
  :inapt-if-not 'systemctl--can-sleep-p
  (interactive)
  (systemctl-manage-logind "Sleep" t))

;;; Poweroff

(defun systemctl--can-poweroff-p ()
  "Check if system can poweroff."
  (string= "yes" (systemctl-manage-logind "CanPowerOff")))

;;;###autoload(autoload 'systemctl-poweroff "systemctl" nil t)
(transient-define-suffix systemctl-poweroff ()
  "Shut down and power-off the system."
  :description "Shutdown"
  :inapt-if-not 'systemctl--can-poweroff-p
  (interactive)
  (systemctl-manage-logind "PowerOff" t))

;;; Reboot

(defun systemctl--can-reboot-p ()
  "Check if system can reboot."
  (string= "yes" (systemctl-manage-logind "CanReboot")))

;;;###autoload(autoload 'systemctl-reboot "systemctl" nil t)
(transient-define-suffix systemctl-reboot ()
  "Reboot the system."
  :description "Reboot"
  :inapt-if-not 'systemctl--can-reboot-p
  (interactive)
  (systemctl-manage-logind "Reboot" t))

;;; Halt

(defun systemctl--can-halt-p ()
  "Check if system can poweroff."
  (string= "yes" (systemctl-manage-logind "CanHalt")))

;;;###autoload(autoload 'systemctl-poweroff "systemctl" nil t)
(transient-define-suffix systemctl-halt ()
  "Shut down but don't power off the system."
  :description "Halt"
  :inapt-if-not 'systemctl--can-halt-p
  (interactive)
  (systemctl-manage-logind "Halt" t))

;;; Reboot to firmware

(defun systemctl--can-reboot-firmware-p ()
  "Check if system can reboot to firmware."
  (string= "yes" (systemctl-manage-logind "CanRebootToFirmwareSetup")))

(defun systemctl--get-reboot-firmware ()
  "Check if system can reboot to firmware."
  (systemctl--logind-property "RebootToFirmwareSetup"))

;;;###autoload(autoload 'systemctl-set-reboot-firmware "systemctl" nil t)
(transient-define-suffix systemctl-set-reboot-firmware (enable)
  "On reboot, ENABLE entry into the system firmware setup.
When called interactively, entry into the firmware setup is toggled."
  :transient 'transient--do-stay
  :inapt-if-not 'systemctl--can-reboot-firmware-p
  :description (lambda ()
                 (format "Reboot to firmware setup (%s)"
                         (if (systemctl--get-reboot-firmware)
                             (propertize "on" 'face 'transient-value)
                           (propertize "off" 'face 'transient-inactive-value))))
  (interactive (list (not (systemctl--get-reboot-firmware))))
  (systemctl-manage-logind "SetRebootToFirmwareSetup" :async nil enable))

;;; Reboot to bootloader

(defun systemctl--can-reboot-bootloader-p ()
  "Check if system can reboot to bootloader menu."
  (string= "yes" (systemctl-manage-logind "CanRebootToBootLoaderMenu")))

(defun systemctl--get-reboot-bootloader ()
  "Check if system can reboot to firmware."
  (let ((timeout (systemctl--logind-property  "RebootToBootLoaderMenu")))
    (unless (= timeout (1- (ash 1 64))) timeout)))

;;;###autoload(autoload 'systemctl-set-reboot-bootloader "systemctl" nil t)
(transient-define-suffix systemctl-set-reboot-bootloader (&optional timeout)
  "On reboot, pause in the bootloader for TIMEOUT seconds."
  :transient 'transient--do-stay
  :inapt-if-not 'systemctl--can-reboot-bootloader-p
  :description (lambda ()
                 (format "Show boot menu (%s)"
                         (if-let* ((timeout (systemctl--get-reboot-bootloader)))
                             (propertize (format "%ds" timeout) 'face 'transient-value)
                           (propertize "off" 'face 'transient-inactive-value))))
  (interactive (list (xor (systemctl--get-reboot-bootloader)
                          (read-string "Timeout (seconds) [5]: " nil nil 5))))
  (systemctl-manage-logind "RebootToBootLoaderMenu" :async nil timeout))

;;; Set next boot

(defun systemctl--can-reboot-entry-p ()
  "Check if system can reboot to bootloader entries."
  (string= "yes" (systemctl-manage-logind "CanRebootToBootLoaderEntry")))

(defun systemctl--get-reboot-entry ()
  "Get the active bootloader entry."
  (when-let* ((e (systemctl--logind-property "RebootToBootLoaderEntry"))
              ((not (string-empty-p e))))
    e))

;;;###autoload(autoload 'systemctl-set-reboot-entry "systemctl" nil t)
(transient-define-suffix systemctl-set-reboot-entry (&optional entry)
  "On reboot, boot the specified boot ENTRY."
  :transient 'transient--do-stay
  :inapt-if-not 'systemctl--can-reboot-entry-p
  :description (lambda ()
                 (format "Reboot to entry (%s)"
                         (if-let* ((entry (systemctl--get-reboot-entry)))
                             (propertize entry 'face 'transient-value)
                           (propertize "none" 'face 'transient-inactive-value))))
  (interactive (list (completing-read "Boot Next: "
                                      (systemctl--logind-property "BootLoaderEntries")
                                      nil t)))
  (systemctl-manage-logind "SetRebootToBootLoaderEntry" :async nil entry))

;;; Power Menu

(transient-define-prefix systemctl-power-menu ()
  "Menu for managing the system's powered state."
  ["Actions"
   [(3 "z" systemctl-sleep)
    (4 "h" systemctl-hibernate)
    (5 "H" systemctl-hybrid-sleep)
    (5 "s" systemctl-suspend)
    (5 "S" systemctl-suspend-then-hibernate)
    ]
   [(3 "r" systemctl-reboot)
    (3 "o" systemctl-poweroff)]]
  ["On next reboot..."
   ("-s" systemctl-set-reboot-firmware)
   ("-m" systemctl-set-reboot-bootloader)
   ("-e" systemctl-set-reboot-entry)])

(provide 'systemctl)
;;; systemctl.el ends here
