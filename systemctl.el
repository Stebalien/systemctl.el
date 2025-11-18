;;; systemctl.el --- Control systemd over D-Bus -*- lexical-binding: t -*-

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

;; This package manages systemd services, power, etc. over D-Bus.

;;; Code:

(require 'transient)
(require 'dbus)

(eval-when-compile (require 'cl-lib))

;;; Constants

(defconst systemctl--dbus-systemd-service "org.freedesktop.systemd1"
  "D-Bus service name for systemd.")

(defconst systemctl--dbus-systemd-path "/org/freedesktop/systemd1"
  "D-Bus object path for the systemd manager.")

(defconst systemctl--dbus-systemd-interface "org.freedesktop.systemd1.Manager"
  "D-Bus interface for the systemd manager.")

(defconst systemctl--dbus-logind-service "org.freedesktop.login1"
  "D-Bus service name for logind.")

(defconst systemctl--dbus-logind-path "/org/freedesktop/login1"
  "D-Bus object path for the logind manager.")

(defconst systemctl--dbus-logind-interface "org.freedesktop.login1.Manager"
  "D-Bus interface for the logind manager.")

(defconst systemctl--dbus-logind-session-interface "org.freedesktop.login1.Session"
  "D-Bus interface for logind sessions.")

(defconst systemctl--dbus-logind-session-path "/org/freedesktop/login1/session/auto"
  "D-Bus object path for the auto-selected session.")

(defgroup systemctl nil
  "Systemd control utilities."
  :prefix "systemctl-"
  :group 'tools)

(defconst systemctl--unit-types
  '( service mount swap socket target device automount timer path slice
     scope )
  "Systemd unit types.")

(defcustom systemctl-unit-types '(service timer)
  "Systemd unit types to display in completion."
  :package-version '(systemctl . "0.0.1")
  :type
  `(choice
    (const :tag "All" t)
    (set :tag "Unit Types"
         ,@(mapcar
            (lambda (type)
              `(const :tag ,(capitalize (symbol-name type)) ,type))
            systemctl--unit-types))))

(defcustom systemctl-manager nil
  "Systemd units to manage: `system', `user', or nil for both."
  :package-version '(systemctl . "0.0.1")
  :type
  '(radio
    :tag "Manage"
    (const :tag "All Services" nil)
    (const :tag "User Services ONLY" user)
    (const :tag "System Services ONLY" system)))

;;; Setup D-Bus error handling.

;; Unfortunately, Emacs swallows errors from asynchronous D-Bus calls
;; by default.

(defun systemctl--dbus-error-handler (ev err)
  "Handle a D-Bus error from a `systemctl' call.
EV is the event that triggered the error, ERR is the error itself.

This handler detects if the error came from this library and, when that
happens, it invokes the error handler with the error."
  (when-let* ((handler (dbus-event-handler ev))
              (_(symbolp handler))
              (real-handler (get handler 'systemctl-dbus-handler)))
    (with-demoted-errors "Error in systemctl D-Bus handler: %S"
      (funcall real-handler (car err) (cdr err)))))
(add-hook 'dbus-event-error-functions #'systemctl--dbus-error-handler)

(defun systemctl--make-dbus-callback (cb)
  "Transform a callback into one that can be passed to `dbus'.
CB must be a function that takes an error symbol as the first argument
and a return-value as the second argument.  If the error symbol is
non-nil, the second argument is the error value.  Otherwise, the
second argument is the return-value."
  (let ((sym (gensym "systemctl--callback")))
    (fset sym (lambda (&rest ret) (funcall cb nil ret)))
    (put sym 'systemctl-dbus-handler cb)
    sym))


;;; Systemctl

(defun systemctl--manage-systemd (manager method async &rest args)
  "Invoke a management METHOD on systemd with the specified ARGS.

MANAGER specifies the manager to operate on.

If ASYNC is non-nil, don't wait for a response and return immediately.
If ASYNC is a function, it will be called when the method completes.
The first argument will be nil on success or, an error symbol on
failure.  The second argument will be the return value or a list of
\\=(ERROR-TYPE ERROR-MESSAGE)."
  (when (and (not noninteractive) (version<= "31.0" emacs-version))
    (setq args (cons :authorizable (cons t args))))
  (when async
    (push (and (functionp async) (systemctl--make-dbus-callback async)) args))
  (apply (if async #'dbus-call-method-asynchronously #'dbus-call-method)
         (pcase manager
           ((or 'system 'nil) :system)
           ('user :session)
           (other (error "Invalid systemd manager selection: %S" other)))
         systemctl--dbus-systemd-service systemctl--dbus-systemd-path
         systemctl--dbus-systemd-interface method args))

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

(defun systemctl--format-unit-display-name (name manager &optional desc)
  "Format unit NAME from MANAGER with DESC for display."
  (propertize (format "%s/%s" manager name)
              'systemctl--unit-description desc))

(defconst systemctl--completion-properties
  `((group-function . ,#'systemctl--completion-group)
    (annotation-function . ,#'systemctl--completion-annotation))
  "Completion properties for `systemctl-read-unit' prompts.")

(defun systemctl--make-completion-table (units)
  "Make a completion table from a list of UNITS."
  (seq-map
   (lambda (unit)
     (cons (apply #'systemctl--format-unit-display-name unit)
           unit))
   units))

(defun systemctl--choose-unit (prompt units)
  "PROMPT for a unit from the given list of UNITS."
  (let ((candidates (systemctl--make-completion-table units)))
    (alist-get
     (completing-read prompt
                      (completion-table-with-metadata
                       candidates systemctl--completion-properties)
                      nil t)
     candidates nil nil #'string=)))

(defun systemctl--parse-filter (filter)
  "Parse FILTER into an alist with the keys `user' `system' and `patterns'."
  (cl-loop with user-only and system-only
           for elt in filter
           if (eq elt 'user)
             do (setq user-only t)
           else if (eq elt 'system)
             do (setq system-only t)
           else if (stringp elt)
             collect elt into patterns
           else if (memq elt systemctl--unit-types)
             collect (format "*.%s" elt) into patterns
           else
             do (error "Unknown filter %S" elt)
           finally return
             `((user . ,(or user-only (not system-only)))
               (system . ,(or system-only (not user-only)))
               (patterns . ,patterns))))

(defun systemctl--list-units (manager patterns)
  "List all loaded units belonging to MANAGER, filtering by PATTERNS if non-empty.
MANAGER is one of `system' or `user'."
  (thread-last
    (systemctl--manage-systemd manager "ListUnitsByPatterns" nil
                               '(:array) ; all states
                               (cons :array patterns))
    (seq-map (pcase-lambda (`(,unit ,desc . ,_))
               (list unit manager desc)))))

(defun systemctl--list-unit-files (manager patterns)
  "List all unit files belonging to MANAGER, filtering by PATTERNS if non-empty.
MANAGER is one of `system' or `user'."
  (thread-last
    (systemctl--manage-systemd manager "ListUnitFilesByPatterns" nil
                               '(:array) ; all states
                               (cons :array patterns))
    (seq-map 'car)
    (seq-map 'file-name-nondirectory)
    (seq-sort 'string-lessp)
    (delete-consecutive-dups)
    (seq-map (lambda (unit) (list unit manager "[unloaded]")))))

(defun systemctl--list-all-units (manager patterns)
  "List all units belonging to MANAGER, filtering by PATTERNS if non-empty.
MANAGER is one of `system' or `user'."
  (thread-last
   (append
    (systemctl--list-units manager patterns)
    (systemctl--list-unit-files manager patterns))
   (seq-sort (lambda (a b) (string-lessp (car a) (car b))))
   (seq-remove
    (let (prev)
      (lambda (item)
        (prog1 (string= (car prev) (car item))
          (setq prev item)))))))

(defun systemctl-read-unit (&optional prompt &rest filter)
  "Prompt for a unit (limited to loaded units).

PROMPT is a string to prompt with.
FILTER limits the units to prompt for. It can contain:
- Any number of symbols/strings limiting the shown unit types (service,
  timer, etc.).
- The symbols `user' and/or `system'. If only one of these is specified,
  only the specified units (user or system) will be shown. By default, all
  units (both user and system) are shown."
  (let-alist (systemctl--parse-filter filter)
    (systemctl--choose-unit (or prompt "Unit: ")
                            (append (when .user (systemctl--list-units 'user .patterns))
                                    (when .system (systemctl--list-units 'system .patterns))))))


(defun systemctl-read-unit-file (&optional prompt &rest filter)
  "Prompt for a file (all known units).
The user will be asked to fill in any templates.

PROMPT is a string to prompt with.
FILTER limits the units to prompt for. It can contain:
- Any number of symbols/strings limiting the shown unit types (service,
  timer, etc.).
- The symbols `user' and/or `system'. If only one of these is specified,
  only the specified units (user or system) will be shown. By default, all
  units (both user and system) are shown."
  (let-alist (systemctl--parse-filter filter)
    (let* ((units
            (append (when .user (systemctl--list-all-units 'user .patterns))
                    (when .system (systemctl--list-all-units 'system .patterns))))
           (unit (systemctl--choose-unit (or prompt "Unit file: ") units)))
    (when (string-suffix-p "@" (file-name-base (car unit)))
      (with-temp-buffer
        (call-process "systemd-escape" nil t nil
                      "--template"
                      (car unit)
                      (read-string "Instance: "))
        (setcar unit (string-trim (buffer-string)))))
    unit)))

(defun systemctl--interactive-filters ()
  "Return the unit filters to be used by-default for completion."
  (if systemctl-manager
      (cons systemctl-manager systemctl-unit-types)
    systemctl-unit-types))

(defun systemctl--interactive-control-args (operation include-files &optional expect-prefix-arg)
  "Query the user for arguments to one of systemctl's unit control operations.
If INCLUDE-FILES is t, unloaded units are included.
If EXPECT-PREFIX-ARG is non-nil, the command takes a prefix argument
in the 3rd position. The value of PREFIX-ARG is appended to the prompt.

Return a list of:

- The selected unit.
- The systemd manager on which to operate.
- If EXPECT-PREFIX-ARG is non-nil, the value of `current-prefix-arg'.
- A callback to report the command's outcome to the user.

OPERATION is the name of the operation (a string). It's used in the
prompt and in error messages."
  (pcase-let ((`(,unit ,manager)
               (apply (if include-files
                          #'systemctl-read-unit-file
                        #'systemctl-read-unit)
                      (concat operation
                              (and current-prefix-arg
                                   expect-prefix-arg)
                              ": ")
                      (systemctl--interactive-filters))))
    `( ,unit
       ,manager
       ,@(when expect-prefix-arg (list current-prefix-arg))
       ,(lambda (err res)
          (when err
            (message "%s %s failed: %s" operation unit (nth 1 res)))))))


;;;###autoload
(defun systemctl-start (unit &optional manager async)
  "Start a UNIT on MANAGER (`user' or `system' (default)).

If ASYNC is non-nil, return immediately.
If ASYNC is a function, it will be called when the method completes.
The first argument will be nil on success and an error symbol on
failure.  The second argument will be the return value or a list of
\\=(ERROR-TYPE ERROR-MESSAGE) where both the ERROR-TYPE and
ERROR-MESSAGE are strings.

On success, return (or pass to the ASYNC callback) the start job for the unit."
  (interactive (systemctl--interactive-control-args "Start" t))
  (systemctl--manage-systemd manager "StartUnit" async unit "replace"))

;;;###autoload
(defun systemctl-stop (unit &optional manager async)
  "Stop a UNIT on MANAGER (`user' or `system' (default)).

If ASYNC is non-nil, return immediately.
If ASYNC is a function, it will be called when the method completes.
The first argument will be nil on success and an error symbol on
failure.  The second argument will be the return value or a list of
\\=(ERROR-TYPE ERROR-MESSAGE) where both the ERROR-TYPE and
ERROR-MESSAGE are strings.

On success, return (or pass to the ASYNC callback) the stop job for the unit."
  (interactive (systemctl--interactive-control-args "Stop" nil))
  (systemctl--manage-systemd manager "StopUnit" async unit "replace"))

;;;###autoload
(defun systemctl-reload (unit &optional manager async)
  "Reload a UNIT on MANAGER (`user' or `system' (default)).

If ASYNC is non-nil, return immediately.
If ASYNC is a function, it will be called when the method completes.
The first argument will be nil on success and an error symbol on
failure.  The second argument will be the return value or a list of
\\=(ERROR-TYPE ERROR-MESSAGE) where both the ERROR-TYPE and
ERROR-MESSAGE are strings.

On success, return (or pass to the ASYNC callback) the reload job for the unit."
  (interactive (systemctl--interactive-control-args "Reload" nil))
  (systemctl--manage-systemd manager "ReloadUnit" async unit "replace"))

;;;###autoload
(defun systemctl-restart (unit &optional manager if-running async)
  "Restart a UNIT on MANAGER (`user' or `system' (default)).
Unless IF-RUNNING is non-nil, the unit will be started if not running.

If ASYNC is non-nil, return immediately.
If ASYNC is a function, it will be called when the method completes.
The first argument will be nil on success and an error symbol on
failure.  The second argument will be the return value or a list of
\\=(ERROR-TYPE ERROR-MESSAGE) where both the ERROR-TYPE and
ERROR-MESSAGE are strings.

On success, return (or pass to the ASYNC callback) the restart job for the unit."
  (interactive (systemctl--interactive-control-args "Restart" nil " (if running)"))
  (systemctl--manage-systemd
   manager (if if-running "TryRestartUnit" "RestartUnit")
   async unit "replace"))

;;;###autoload
(defun systemctl-reload-or-restart (unit &optional manager if-running async)
  "Reload or restart a UNIT on MANAGER (`user' or `system' (default)).
Unless IF-RUNNING is non-nil, the unit will be started if not running.

If ASYNC is non-nil, return immediately.
If ASYNC is a function, it will be called when the method completes.
The first argument will be nil on success and an error symbol on
failure.  The second argument will be the return value or a list of
\\=(ERROR-TYPE ERROR-MESSAGE) where both the ERROR-TYPE and
ERROR-MESSAGE are strings.

On success, return (or pass to the ASYNC callback) the reload/restart
job for the unit."
  (interactive (systemctl--interactive-control-args "Reload or restart" nil " (if running)"))
  (systemctl--manage-systemd
   manager
   (if if-running "ReloadOrTryRestartUnit" "ReloadOrRestartUnit")
   async unit "replace"))

;;;###autoload
(defun systemctl-daemon-reload (&optional manager)
  "Reload the systemd configuration on MANAGER (`user' or `system' (default))."
  (interactive (list (or systemctl-manager
                         (pcase (read-answer
                                 "Reload the [s]ystem or [u]ser daemon? "
                                 '(("system" ?s "reload the system daemon")
                                   ("user" ?u "reload the user daemon")
                                   ("quit" ?q "abort")))
                           ("system" 'system)
                           ("user" 'user)
                           (_ (keyboard-quit))))))
  (systemctl--manage-systemd manager "Reload" 'async))


(defun systemctl--format-link-ops (ops)
  "Format link OPS where each op is a list of (OP FROM TO)."
  (string-join
   (mapcar (pcase-lambda (`(,action ,from ,to))
             (if (string-empty-p to)
                 (format "%s %s" action (abbreviate-file-name from))
               (format "%s %s -> %s" action
                       (abbreviate-file-name from)
                       (abbreviate-file-name to))))
           ops)
   "; "))

(defun systemctl--interactive-link-args (command)
  "Query the user for arguments to one of the systemctl \"link\" commands.
Returns a list of:

- The selected unit.
- The systemd manager on which to operate.
- A boolean indicating whether to perform the link operation for the
  current session only.
- A callback to report the command's outcome to the user.

COMMAND is the name of the command (a string)."
  (pcase-let ((`(,unit ,manager)
               (apply #'systemctl-read-unit-file
                      (concat (capitalize command) ": ")
                      (systemctl--interactive-filters))))
    (list unit manager current-prefix-arg
          (lambda (err res)
            (if err
                (message "%s %s failed: %s" command unit (nth 1 res))
              (when (length= res 1) (push t res))
              (pcase res
                (`(nil ,_)
                 (message "%s %s: unit has no install section" command unit))
                (`(t nil) (message "%s %s: nothing to do" command unit))
                (`(t ,ops)
                 (message "%s %s: %s" command unit
                          (systemctl--format-link-ops ops)))))))))

;;;###autoload
(defun systemctl-enable (unit &optional manager runtime async)
  "Enable a UNIT on MANAGER (`user' or `system' (default)).
With prefix-argument RUNTIME, enable only for this session.

If ASYNC is non-nil, return immediately.
If ASYNC is a function, it will be called when the method completes.
The first argument will be nil on success and an error symbol on
failure.  The second argument will be the return value or a list of
\\=(ERROR-TYPE ERROR-MESSAGE) where both the ERROR-TYPE and
ERROR-MESSAGE are strings.

On success, return (or pass to the ASYNC callback) a list of:

1. A boolean indicating whether or not the target unit even has an
Install section.
2. A list of (un)link operations performed where each element is a list
of strings: (OPERATION FROM TO)."
  (interactive (systemctl--interactive-link-args "enable"))
  (systemctl--manage-systemd manager "EnableUnitFiles"
                             async (list unit) runtime nil))

;;;###autoload
(defun systemctl-disable (unit &optional manager runtime async)
  "Disable a UNIT on MANAGER (`user' or `system' (default)).
With prefix-argument RUNTIME, disable only for this session.

If ASYNC is non-nil, return immediately.
If ASYNC is a function, it will be called when the method completes.
The first argument will be nil on success and an error symbol on
failure.  The second argument will be the return value or a list of
\\=(ERROR-TYPE ERROR-MESSAGE) where both the ERROR-TYPE and
ERROR-MESSAGE are strings.

On success, return (or pass to the ASYNC callback as a list of length
one) a list of (un)link operations performed where each element is a
list of strings: (OPERATION FROM TO)."
  (interactive (systemctl--interactive-link-args "disable"))
  (systemctl--manage-systemd manager "DisableUnitFiles"
                             async (list unit) runtime))

;;;###autoload
(defun systemctl-mask (unit &optional manager runtime async)
  "Mask a UNIT on MANAGER (`user' or `system' (default)).
With prefix-argument RUNTIME, mask only for this session.

If ASYNC is non-nil, return immediately.
If ASYNC is a function, it will be called when the method completes.
The first argument will be nil on success and an error symbol on
failure.  The second argument will be the return value or a list of
\\=(ERROR-TYPE ERROR-MESSAGE) where both the ERROR-TYPE and
ERROR-MESSAGE are strings.

On success, return (or pass to the ASYNC callback as a list of length
one) a list of (un)link operations performed where each element is a
list of strings: (OPERATION FROM TO)."
  (interactive (systemctl--interactive-link-args "mask"))
  (systemctl--manage-systemd manager "MaskUnitFiles"
                             async (list unit) runtime nil))

;;;###autoload
(defun systemctl-unmask (unit &optional manager runtime async)
  "Unmask UNIT on MANAGER (`user' or `system' (default)).
With prefix-argument RUNTIME, unmask only for this session.

If ASYNC is non-nil, return immediately.
If ASYNC is a function, it will be called when the method completes.
The first argument will be nil on success and an error symbol on
failure.  The second argument will be the return value or a list of
\\=(ERROR-TYPE ERROR-MESSAGE) where both the ERROR-TYPE and
ERROR-MESSAGE are strings.

On success, return (or pass to the ASYNC callback as a list of length
one) a list of (un)link operations performed where each element is a
list of strings: (OPERATION FROM TO)."
  (interactive (systemctl--interactive-link-args "unmask"))
  (systemctl--manage-systemd manager "UnmaskUnitFiles"
                             async (list unit) runtime))

;;; Loginctl

(defun systemctl--manage-logind (method async &rest args)
  "Invoke a management METHOD on logind with the specified ARGS.

If ASYNC is non-nil, don't wait for a response and return immediately.
If ASYNC is a function, it will be called when the method completes.
The first argument will be nil on success or, an error symbol on
failure.  The second argument will be the return value or a list of
\\=(ERROR-TYPE ERROR-MESSAGE)."
  (when (and (not noninteractive) (version<= "31.0" emacs-version))
    (setq args (cons :authorizable (cons t args))))
  (when async
    (push (and (functionp async) (systemctl--make-dbus-callback async)) args))
  (apply (if async #'dbus-call-method-asynchronously #'dbus-call-method)
         :system systemctl--dbus-logind-service
         systemctl--dbus-logind-path
         systemctl--dbus-logind-interface method args))

(defun systemctl--logind-property (name)
  "Get the logind property named NAME."
  (dbus-get-property
   :system systemctl--dbus-logind-service
   systemctl--dbus-logind-path
   systemctl--dbus-logind-interface name))

;;; Lock/Unlock

(defun systemctl--lock-unlock-common (action session async)
  "Lock or Unlock (ACTION) the specified SESSION.

If ASYNC is non-nil, return immediately.
If ASYNC is a function, it will be called when the method completes.
The first argument will be nil on success and an error symbol on
failure.  The second argument will be the nil or a list of
\\=(ERROR-TYPE ERROR-MESSAGE) where both the ERROR-TYPE and
ERROR-MESSAGE are strings."
  (cond
   ((null session)
    (if async
        (dbus-call-method-asynchronously
         :system systemctl--dbus-logind-service
         systemctl--dbus-logind-session-path
         systemctl--dbus-logind-session-interface
         action
         (when (functionp async)
           (systemctl--make-dbus-callback async)))
      (dbus-call-method
       :system systemctl--dbus-logind-service
       systemctl--dbus-logind-session-path
       systemctl--dbus-logind-session-interface
       action)))
   ((eq session t) (systemctl--manage-logind (concat action "Sessions") async))
   ((stringp session) (systemctl--manage-logind (concat action "Session") async session))
   (t (error "Invalid `session' argument"))))

;;;###autoload
(defun systemctl-lock (&optional session async)
  "Lock the current or specified SESSION.
If SESSION is t or when invoked with a prefix argument, lock all
sessions (requires authentication).

If ASYNC is non-nil, return immediately.
If ASYNC is a function, it will be called when the method completes.
The first argument will be nil on success and an error symbol on
failure.  The second argument will be the nil or a list of
\\=(ERROR-TYPE ERROR-MESSAGE) where both the ERROR-TYPE and
ERROR-MESSAGE are strings."
  (interactive (list (not (null current-prefix-arg)) 'async))
  (systemctl--lock-unlock-common "Lock" session async))

;;;###autoload
(defun systemctl-unlock (&optional session async)
  "Unlock the current or specified SESSION.
If SESSION is t or when invoked with a prefix argument, unlock all
sessions (requires authentication).

If ASYNC is non-nil, return immediately.
If ASYNC is a function, it will be called when the method completes.
The first argument will be nil on success and an error symbol on
failure.  The second argument will be the nil or a list of
\\=(ERROR-TYPE ERROR-MESSAGE) where both the ERROR-TYPE and
ERROR-MESSAGE are strings."
  (interactive (list (not (null current-prefix-arg)) 'async))
  (systemctl--lock-unlock-common "Unlock" session async))

;;; Suspend

(defun systemctl--can-suspend-p ()
  "Check if system can suspend."
  (string= "yes" (systemctl--manage-logind "CanSuspend" nil)))

;;;###autoload(autoload 'systemctl-suspend "systemctl" nil t)
(transient-define-suffix systemctl-suspend ()
  "Suspend the system."
  :description "Suspend"
  :inapt-if-not 'systemctl--can-suspend-p
  (interactive)
  (systemctl--manage-logind "Suspend" 'async t))

;;; Hibernate

(defun systemctl--can-hibernate-p ()
  "Check if system can hibernate."
  (string= "yes" (systemctl--manage-logind "CanHibernate" nil)))

;;;###autoload(autoload 'systemctl-hibernate "systemctl" nil t)
(transient-define-suffix systemctl-hibernate ()
  "Hibernate the system."
  :description "Hibernate"
  :inapt-if-not 'systemctl--can-hibernate-p
  (interactive)
  (systemctl--manage-logind "Hibernate" 'async t))

;;; Hybrid-sleep

(defun systemctl--can-hybrid-sleep-p ()
  "Check if system can hybrid-sleep."
  (string= "yes" (systemctl--manage-logind "CanHybridSleep" nil)))

;;;###autoload(autoload 'systemctl-hybrid-sleep "systemctl" nil t)
(transient-define-suffix systemctl-hybrid-sleep ()
  "Hybrid suspend/sleep (suspend and hibernate at the same time)."
  :description "Hybrid sleep (suspend to both)"
  :inapt-if-not 'systemctl--can-hybrid-sleep-p
  (interactive)
  (systemctl--manage-logind "HybridSleep" 'async t))

;;; Suspend then hibernate

(defun systemctl--can-suspend-then-hibernate-p ()
  "Check if system can suspend-then-hibernate."
  (string= "yes" (systemctl--manage-logind "CanSuspendThenHibernate" nil)))

;;;###autoload(autoload 'systemctl-suspend-then-hibernate "systemctl" nil t)
(transient-define-suffix systemctl-suspend-then-hibernate ()
  "Suspend now then hibernate once the system reaches a critical battery level."
  :description "Suspend Then Hibernate"
  :inapt-if-not 'systemctl--can-suspend-then-hibernate-p
  (interactive)
  (systemctl--manage-logind "SuspendThenHibernate" 'async t))

;;; Sleep

(defun systemctl--can-sleep-p ()
  "Check if system can sleep (using the default method)."
  (string= "yes" (systemctl--manage-logind "CanSleep" nil)))

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
  (systemctl--manage-logind "Sleep" 'async :uint64 0))

;;; Poweroff

(defun systemctl--can-poweroff-p ()
  "Check if system can poweroff."
  (string= "yes" (systemctl--manage-logind "CanPowerOff" nil)))

;;;###autoload(autoload 'systemctl-poweroff "systemctl" nil t)
(transient-define-suffix systemctl-poweroff ()
  "Shut down and power-off the system."
  :description "Shutdown"
  :inapt-if-not 'systemctl--can-poweroff-p
  (interactive)
  (systemctl--manage-logind "PowerOff" 'async t))

;;; Reboot

(defun systemctl--can-reboot-p ()
  "Check if system can reboot."
  (string= "yes" (systemctl--manage-logind "CanReboot" nil)))

;;;###autoload(autoload 'systemctl-reboot "systemctl" nil t)
(transient-define-suffix systemctl-reboot ()
  "Reboot the system."
  :description "Reboot"
  :inapt-if-not 'systemctl--can-reboot-p
  (interactive)
  (systemctl--manage-logind "Reboot" 'async t))

;;; Halt

(defun systemctl--can-halt-p ()
  "Check if system can halt."
  (string= "yes" (systemctl--manage-logind "CanHalt" nil)))

;;;###autoload(autoload 'systemctl-halt "systemctl" nil t)
(transient-define-suffix systemctl-halt ()
  "Shut down but don't power off the system."
  :description "Halt"
  :inapt-if-not 'systemctl--can-halt-p
  (interactive)
  (systemctl--manage-logind "Halt" 'async t))

;;; Reboot to firmware

(defun systemctl--can-reboot-firmware-p ()
  "Check if system can reboot to firmware."
  (string= "yes" (systemctl--manage-logind "CanRebootToFirmwareSetup" nil)))

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
  (systemctl--manage-logind "SetRebootToFirmwareSetup" nil enable))

;;; Reboot to bootloader

(defun systemctl--can-reboot-bootloader-p ()
  "Check if system can reboot to bootloader menu."
  (string= "yes" (systemctl--manage-logind "CanRebootToBootLoaderMenu" nil)))

(defun systemctl--get-reboot-bootloader ()
  "Check if system can reboot to firmware."
  (let ((timeout (systemctl--logind-property "RebootToBootLoaderMenu")))
    (unless (= timeout (1- (ash 1 64))) timeout)))

;;;###autoload(autoload 'systemctl-set-reboot-bootloader "systemctl" nil t)
(transient-define-suffix systemctl-set-reboot-bootloader (&optional timeout)
  "On reboot, pause in the bootloader for TIMEOUT seconds."
  :transient t
  :inapt-if-not 'systemctl--can-reboot-bootloader-p
  :description (lambda ()
                 (format "Show boot menu (%s)"
                         (if-let* ((timeout (systemctl--get-reboot-bootloader)))
                             (propertize (seconds-to-string timeout 'expanded t)
                                         'face 'transient-value)
                           (propertize "off" 'face 'transient-inactive-value))))
  (interactive (list (xor (systemctl--get-reboot-bootloader)
                          (read-string "Timeout (seconds) [5]: " nil nil 5))))
  (systemctl--manage-logind "RebootToBootLoaderMenu" nil timeout))

;;; Set next boot

(defun systemctl--can-reboot-entry-p ()
  "Check if system can reboot to bootloader entries."
  (string= "yes" (systemctl--manage-logind "CanRebootToBootLoaderEntry" nil)))

(defun systemctl--get-reboot-entry ()
  "Get the active bootloader entry."
  (when-let* ((e (systemctl--logind-property "RebootToBootLoaderEntry"))
              ((not (string-empty-p e))))
    e))

;;;###autoload(autoload 'systemctl-set-reboot-entry "systemctl" nil t)
(transient-define-suffix systemctl-set-reboot-entry (&optional entry)
  "On reboot, boot the specified boot ENTRY."
  :transient t
  :inapt-if-not 'systemctl--can-reboot-entry-p
  :description (lambda ()
                 (format "Reboot to entry (%s)"
                         (if-let* ((entry (systemctl--get-reboot-entry)))
                             (propertize entry 'face 'transient-value)
                           (propertize "none" 'face 'transient-inactive-value))))
  (interactive (list (completing-read "Boot Next: "
                                      (systemctl--logind-property "BootLoaderEntries")
                                      nil t)))
  (systemctl--manage-logind "SetRebootToBootLoaderEntry" nil entry))

;;; Power Menu

;;;###autoload(autoload 'systemctl-power-menu "systemctl" nil t)
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
   (4 "-s" systemctl-set-reboot-firmware)
   (4 "-m" systemctl-set-reboot-bootloader)
   (4 "-e" systemctl-set-reboot-entry)])

(provide 'systemctl)
;;; systemctl.el ends here
