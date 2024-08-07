;;; systemctl.el --- Systemctl -*- lexical-binding: t -*-

;; Copyright 2020 Steven Allen <steven@stebalien.com>

;; Author: Steven Allen <steven@stebalien.com>
;; URL: https://github.com/Stebalien/systemctl.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.0"))
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
(require 'dbus)

(eval-when-compile (require 'cl-lib))

(defgroup systemctl nil
  "Systemd control utilities."
  :version "0.0.1"
  :group 'tools)

(defcustom systemctl-unit-types '("service" "timer")
  "Systemd unit types to display in completion."
  :group 'systemctl
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
  (when (version<= "31.0" emacs-version)
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

(defun systemctl--logind-manage (method &rest args)
  "Invoke a management METHOD on logind with the specified ARGS."
  (apply #'dbus-call-method-asynchronously
         :system "org.freedesktop.login1"
         "/org/freedesktop/login1"
         "org.freedesktop.login1.Manager" method nil args))

(defun systemctl--logind-graphical-session ()
  "Return the graphical session."
  (car (dbus-get-property
        :system "org.freedesktop.login1"
        "/org/freedesktop/login1/user/self"
        "org.freedesktop.login1.User" "Display")))

(defun systemctl--lock-unlock-common (action &optional session)
  "Lock or Unlock (ACTION) the specified SESSION."
  (cond
   ((null session) (if (display-graphic-p)
                       (systemctl--lock-unlock-common
                        action
                        (systemctl--logind-graphical-session))
                     (dbus-call-method-asynchronously
                      :system "org.freedesktop.login1"
                      "/org/freedesktop/login1/session/self"
                      "org.freedesktop.login1.Session" action nil)))
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

;;;###autoload
(defun systemctl-suspend ()
  "Suspend the system."
  (interactive)
  (systemctl--logind-manage "Suspend" t))

;;;###autoload
(defun systemctl-hibernate ()
  "Hibernate the system."
  (interactive)
  (systemctl--logind-manage "Hibernate" t))

;;;###autoload
(defun systemctl-hybrid-sleep ()
  "Hybrid suspend/sleep."
  (interactive)
  (systemctl--logind-manage "HybridSleep" t))

;;;###autoload
(defun systemctl-poweroff ()
  "Poweroff the system."
  (interactive)
  (systemctl--logind-manage "PowerOff" t))

;;;###autoload
(defun systemctl-reboot ()
  "Reboot the system."
  (interactive)
  (systemctl--logind-manage "Reboot" t))

;;;###autoload
(defun systemctl-logout ()
  "Poweroff the system."
  (interactive)
  (systemctl-stop "graphical-session.target" :user t))

(provide 'systemctl)
;;; systemctl.el ends here
