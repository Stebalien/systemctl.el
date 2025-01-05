;;; systemctl.el --- Systemctl -*- lexical-binding: t -*-

;; Copyright 2020 Steven Allen <steven@stebalien.com>

;; Author: Steven Allen <steven@stebalien.com>
;; URL: https://github.com/Stebalien/systemctl.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.0"))
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

(cl-defun systemctl-manage (method &rest args &key user async &allow-other-keys)
  "Invoke a systemctl management METHOD with the specified ARGS.

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


(defun systemctl--list-unit-files (&optional user)
  "List all unit files.

Lists system units by default, or USER units when specified."
  (thread-last
    (systemctl-manage "ListUnitFiles" :user user)
    (seq-map 'car)
    (seq-map 'file-name-nondirectory)
    (seq-filter (lambda (i) (member (file-name-extension i) systemctl-unit-types)))
    (seq-sort 'string-lessp)
    (delete-consecutive-dups)
    (seq-map (lambda (unit) (list (format "%-8s %s"
                                     (if user "user" "system")
                                     unit)
                             unit :user user)))))

(defun systemctl--list-units (&optional user)
  "List all units.

Lists system units matching `systemctl-unit-types' by default,
or USER units when specified."
  (thread-last
    (systemctl-manage "ListUnits" :user user)
    (seq-filter (lambda (i) (member (file-name-extension (car i)) systemctl-unit-types)))
    (seq-map (pcase-lambda (`(,unit ,desc . ,_))
               (list (format "%-8s %s - %s"
                             (if user "user" "system")
                             unit
                             desc)
                     unit :user user)))))

;;;###autoload
(cl-defun systemctl-start (unit &key user)
  "Start a UNIT.

Specify USER to manage a user unit."
  (interactive (systemctl--prompt-unit-file "Start Service: "))
  (systemctl-manage "StartUnit" unit "replace" :user user :async t))

(defun systemctl--prompt-unit (prompt)
  "Prompt for a unit (limited to loaded units).

PROMPT is a string to prompt with.
Bind or customize `systemctl-unit-types' to limit the allowed unit types."
  (let ((units (append (systemctl--list-units t) (systemctl--list-units nil))))
    (cdr (assoc (completing-read prompt units nil t) units))))

(defun systemctl--prompt-unit-file (prompt)
  "Prompt for a unit file (all known units).

PROMPT is a string to prompt with.
Bind or customize `systemctl-unit-types' to limit the allowed unit types."
  (let* ((units (append (systemctl--list-unit-files t) (systemctl--list-unit-files nil)))
         (tuple (cdr (assoc (completing-read prompt units nil t) units))))
    (if (string-suffix-p "@" (file-name-base (car tuple)))
        (cons (with-temp-buffer
                (call-process "systemd-escape" nil t nil
                              "--template"
                              (car tuple)
                              (read-string "Instance: "))
                (string-trim (buffer-string)))
              (cdr tuple))
      tuple)))

;;;###autoload
(cl-defun systemctl-stop (unit &key user)
  "Start a systemd UNIT.

Specify USER to restart a user unit."
  (interactive (systemctl--prompt-unit "Stop Service: "))
  (systemctl-manage "StopUnit" unit "replace" :user user :async t))

;;;###autoload
(cl-defun systemctl-restart (unit &key user try)
  "Restart the systemd UNIT.

Specify USER to restart a user unit.
Specify TRY to try to restart the unit, if and only if it's already running."
  (interactive (systemctl--prompt-unit "Restart Service: "))
  (systemctl-manage
   (if try "TryRestartUnit" "RestartUnit")
   :user user
   :async t
   unit "replace"))

;;;###autoload
(cl-defun systemctl-reload (unit &key user or-restart)
  "Reload the systemd UNIT.

Specify USER to reload a user unit.
Specify OR-RESTART to restart the unit if it cannot be reloaded."
  (interactive (systemctl--prompt-unit "Reload Service: "))
  (systemctl-manage
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
  (interactive)
  (systemctl-manage "Reload" :user user :async t))

(cl-defun systemctl--logind-manage (method &rest args &key async &allow-other-keys)
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
   ((eq session t) (systemctl--logind-manage (concat action "Sessions")))
   ((stringp session) (systemctl--logind-manage (concat action "Session") session))
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
  (string= "yes" (systemctl--logind-manage "CanSuspend")))

;;;###autoload(autoload 'systemctl-suspend "systemctl" nil t)
(transient-define-suffix systemctl-suspend ()
  "Suspend the system."
  :description "Suspend"
  :inapt-if-not 'systemctl--can-suspend-p
  (interactive)
  (systemctl--logind-manage "Suspend" t))

;;; Hibernate

(defun systemctl--can-hibernate-p ()
  "Check if system can hibernate."
  (string= "yes" (systemctl--logind-manage "CanHibernate")))

;;;###autoload(autoload 'systemctl-hibernate "systemctl" nil t)
(transient-define-suffix systemctl-hibernate ()
  "Hibernate the system."
  :description "Hibernate"
  :inapt-if-not 'systemctl--can-hibernate-p
  (interactive)
  (systemctl--logind-manage "Hibernate" t))

;;; Hybrid-sleep

(defun systemctl--can-hybrid-sleep-p ()
  "Check if system can hybrid-sleep."
  (string= "yes" (systemctl--logind-manage "CanHybridSleep")))

;;;###autoload(autoload 'systemctl-hybrid-sleep "systemctl" nil t)
(transient-define-suffix systemctl-hybrid-sleep ()
  "Hybrid suspend/sleep (suspend and hibernate at the same time)."
  :description "Hybrid sleep (suspend to both)"
  :inapt-if-not 'systemctl--can-hybrid-sleep-p
  (interactive)
  (systemctl--logind-manage "HybridSleep" t))

;;; Suspend then hibernate

(defun systemctl--can-suspend-then-hibernate-p ()
  "Check if system can suspend-then-hibernate."
  (string= "yes" (systemctl--logind-manage "CanSuspendThenHibernate")))

;;;###autoload(autoload 'systemctl-suspend-then-hibernate "systemctl" nil t)
(transient-define-suffix systemctl-suspend-then-hibernate ()
  "Suspend now then hibernate once the system reaches a critical battery level."
  :description "Suspend Then Hibernate"
  :inapt-if-not 'systemctl--can-suspend-then-hibernate-p
  (interactive)
  (systemctl--logind-manage "SuspendThenHibernate" t))

;;; Sleep

(defun systemctl--can-sleep-p ()
  "Check if system can sleep (using the default method)."
  (string= "yes" (systemctl--logind-manage "CanSleep")))

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
  (systemctl--logind-manage "Sleep" t))

(defun systemctl--can-poweroff-p ()
  "Check if system can poweroff."
  (string= "yes" (systemctl--logind-manage "CanPowerOff")))

;;;###autoload(autoload 'systemctl-poweroff "systemctl" nil t)
(transient-define-suffix systemctl-poweroff ()
  "Shut down and power-off the system."
  :description "Shutdown"
  :inapt-if-not 'systemctl--can-poweroff-p
  (interactive)
  (systemctl--logind-manage "PowerOff" t))

;;; Reboot

(defun systemctl--can-reboot-p ()
  "Check if system can reboot."
  (string= "yes" (systemctl--logind-manage "CanReboot")))

;;;###autoload(autoload 'systemctl-reboot "systemctl" nil t)
(transient-define-suffix systemctl-reboot ()
  "Reboot the system."
  :description "Reboot"
  :inapt-if-not 'systemctl--can-reboot-p
  (interactive)
  (systemctl--logind-manage "Reboot" t))

;;; Reboot to firmware

(defun systemctl--can-reboot-firmware-p ()
  "Check if system can reboot to firmware."
  (string= "yes" (systemctl--logind-manage "CanRebootToFirmwareSetup")))

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
  (systemctl--logind-manage "SetRebootToFirmwareSetup" :async nil enable))

;;; Reboot to bootloader

(defun systemctl--can-reboot-bootloader-p ()
  "Check if system can reboot to bootloader menu."
  (string= "yes" (systemctl--logind-manage "CanRebootToBootLoaderMenu")))

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
  (systemctl--logind-manage "RebootToBootLoaderMenu" :async nil timeout))

;;; Set next boot

(defun systemctl--can-reboot-entry-p ()
  "Check if system can reboot to bootloader entries."
  (string= "yes" (systemctl--logind-manage "CanRebootToBootLoaderEntry")))

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
  (systemctl--logind-manage "SetRebootToBootLoaderEntry" :async nil entry))

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
