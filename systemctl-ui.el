;;; systemctl-ui.el --- Tabulated list view for systemd units -*- lexical-binding: t -*-

;; Copyright 2025 Steven Allen <steven@stebalien.com>

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

;; A tabulated list view for systemd units.

;;; Code:

(require 'systemctl)
(require 'tabulated-list)
(require 'dbus)

(defgroup systemctl-ui nil
  "Tabulated list view for systemd units."
  :prefix "systemctl-ui-"
  :group 'systemctl)

(defface systemctl-ui-enabled
  '((t :inherit success))
  "Face for enabled units."
  :group 'systemctl-ui)

(defface systemctl-ui-indirect
  '((t :inherit success))
  "Face for indirectly enabled units."
  :group 'systemctl-ui)

(defface systemctl-ui-disabled
  '((t :inherit shadow))
  "Face for disabled units."
  :group 'systemctl-ui)

(defface systemctl-ui-bad
  '((t :inherit (bold error)))
  "Face for units in bad state."
  :group 'systemctl-ui)

(defface systemctl-ui-masked
  '((t :inherit (shadow italic)))
  "Face for masked units."
  :group 'systemctl-ui)

(defface systemctl-ui-active
  '((t :inherit success))
  "Face for active units."
  :group 'systemctl-ui)

(defface systemctl-ui-inactive
  '((t :inherit shadow))
  "Face for inactive units."
  :group 'systemctl-ui)

(defface systemctl-ui-failed
  '((t :inherit (warning italic)))
  "Face for units in failed or other problematic states."
  :group 'systemctl-ui)

(defcustom systemctl-ui-manager systemctl-manager
  "Systemd manager to display units from.
Inherits from `systemctl-manager' by default."
  :type (get 'systemctl-manager 'custom-type)
  :package-version '(systemctl . "0.0.1"))

(defvar-keymap systemctl-ui-mode-map
  :parent tabulated-list-mode-map
  :doc "Keymap for `systemctl-ui-mode'."
  "s" #'systemctl-ui-start
  "S" #'systemctl-ui-stop
  "r" #'systemctl-ui-restart
  "R" #'systemctl-ui-reload-or-restart
  "e" #'systemctl-ui-enable
  "d" #'systemctl-ui-disable
  "m" #'systemctl-ui-mask
  "M" #'systemctl-ui-unmask
  "RET" #'systemctl-ui-status)

(defvar-local systemctl-ui--units nil
  "List of systemd units.")

(defvar-local systemctl-ui--manager nil
  "Manager (system/user).")

(defvar-local systemctl-ui--reloading nil
  "Non-nil when systemd is reloading.")

(defvar-local systemctl-ui--dbus-subscriptions nil
  "List of active D-Bus signal subscriptions.")

(defvar-local systemctl-ui--dbus-initialized nil
  "Whether D-Bus subscriptions have been initialized for this buffer.")

(define-derived-mode systemctl-ui-mode tabulated-list-mode "Systemctl"
  "Major mode for browsing systemd units."
  (setq tabulated-list-format
        [("Unit" 40 t)
         ("Enabled" 12 t)
         ("Active" 12 t)
         ("State" 12 t)
         ("Description" 0 nil)]
        tabulated-list-entries #'systemctl-ui--entries
        tabulated-list-sort-key '("Unit" . nil)
        tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun systemctl-ui--update-units ()
  "Collect all units according to current filters."
  (setq systemctl-ui--units
        (mapcar
         (lambda (unit)
           (let-alist unit
             (cons (or .dbus-path .filename) unit)))
         (systemctl--list-all-units systemctl-ui--manager nil)))
  (tabulated-list-revert))

(defun systemctl-ui--entries ()
  "Return formatted `systemctl-ui' entries."
  (cl-loop
   for (id . unit) in systemctl-ui--units
   collect
   (let-alist unit
     (list
      id
      (vector
       .unit
       (propertize
        (or .enablement-state "-")
        'face
        (pcase .enablement-state
          ("enabled" 'systemctl-ui-enabled)
          ("indirect" 'systemctl-ui-indirect)
          ("disabled" 'systemctl-ui-disabled)
          ("bad" 'systemctl-ui-bad)
          ("masked" 'systemctl-ui-masked)
          (_ 'default)))
       (propertize
        (or .active-state "inactive")
        'face
        (pcase .active-state
          ("active" 'systemctl-ui-active)
          ((or 'nil "inactive") 'systemctl-ui-inactive)
          (_ 'systemctl-ui-failed)))
       (or .active-substate "-")
       (or .description ""))))))

(defun systemctl-ui--get-current-unit ()
  "Get the unit at point."
  (cdr (assoc (tabulated-list-get-id) systemctl-ui--units)))

(defmacro systemctl-ui--defcmd (action has-arg is-link-cmd)
  "Define a `systemctl-ui' command.
The newly defined command calls `systemctl-ACTION' with:

- The unit at point.
- The manager of the unit at point.
- If HAS-ARG is t, the `current-prefix-arg'.
- The appropriate callback to provide interactive feedback based on
the value of IS-LINK-CMD."
  (let* ((fn-name (intern (format "systemctl-ui-%S" action)))
         (cmd-name (intern (format "systemctl-%S" action)))
         (op (let ((words (string-split (symbol-name action) "-")))
               (cl-callf capitalize (car words))
               (string-join words " ")))
         (cb (if is-link-cmd
                 'systemctl--interactive-link-callback
               'systemctl--interactive-control-callback))
         (doc (format "%s the unit at point." op)))
    `(defun ,fn-name ,(and has-arg '(&optional arg))
       ,doc
       (interactive ,(and has-arg '(current-prefix-arg)) systemctl-ui-mode)
       (if-let* ((unit-info (systemctl-ui--get-current-unit)))
           (let-alist unit-info
             (,cmd-name .unit .manager
                        ,@(and has-arg '(arg))
                        (,cb ,op .unit)))
         (user-error "No unit at point")))))

(systemctl-ui--defcmd start nil nil)
(systemctl-ui--defcmd stop nil nil)
(systemctl-ui--defcmd reload nil nil)
(systemctl-ui--defcmd restart t nil)
(systemctl-ui--defcmd reload-or-restart t nil)
(systemctl-ui--defcmd enable t t)
(systemctl-ui--defcmd disable t t)
(systemctl-ui--defcmd mask t t)
(systemctl-ui--defcmd unmask t t)

(defun systemctl-ui-status ()
  "Show detailed information about the unit at point."
  (interactive)
  (if-let* ((unit-info (systemctl-ui--get-current-unit)))
      (let-alist unit-info
        (let ((buffer (get-buffer-create (format "*systemctl (%s): %s*" .manager .unit))))
          (with-current-buffer buffer
            (erase-buffer)
            (insert " ") ; Keep the buffer from scrolling.
            (goto-char (point-min))
            (make-process
             :name (format "systemctl %s" .unit)
             :buffer (current-buffer)
             :noquery t
             :command
             `("systemctl" ,@(when (eq .manager 'user) (list "--user")) "status" "--" ,.unit)
             :sentinel 'ignore)
            (view-mode))
          (pop-to-buffer buffer)))
    (user-error "No unit at point")))

(defun systemctl-ui--setup-dbus ()
  "Set up D-Bus subscriptions for systemd events."
  (when (and (not systemctl-ui--dbus-initialized)
             (featurep 'dbusbind))
    (condition-case err
        (progn
          (systemctl-ui--subscribe)
          (setq systemctl-ui--dbus-initialized t))
      (error
       (message "Failed to set up D-Bus subscriptions: %s" (error-message-string err))))))

(defmacro systemctl-ui--signal-handler (arg-list &rest body)
  "Define a new signal handler function.

ARG-LIST and BODY are the same as in `lambda`.

The current buffer will be captured by the signal handler and:

- If possible, the buffer's window will be selected before evaluating BODY.
- Otherwise, the buffer will be made current before evaluating BODY."
  (declare (indent 1))
  (let ((bufvar (gensym "buf"))
        (fnname (gensym "fn")))
  `(let ((,bufvar (current-buffer))
         (,fnname (lambda ,arg-list ,@body)))
     (lambda ,arg-list
       (if-let* ((win (get-buffer-window buf t)))
           (with-selected-window win
             (funcall ,fnname ,@arg-list))
         (with-current-buffer buf
           (funcall ,fnname ,@arg-list)))))))

(defun systemctl-ui--subscribe ()
  "Subscribe to systemd events on BUS (either :system or :session)."
  (let ((bus (systemctl--bus-for-manager systemctl-ui--manager))
        (buf (current-buffer)))
    (dbus-call-method
     bus
     "org.freedesktop.systemd1"
     "/org/freedesktop/systemd1"
     "org.freedesktop.systemd1.Manager"
     "Subscribe")

    ;; NOTE: Disabled because this fires when enumerating properties on a non-existent unit, leading
    ;; to an infinite loop.
    ;;(push (dbus-register-signal
    ;;       bus
    ;;       "org.freedesktop.systemd1"
    ;;       "/org/freedesktop/systemd1"
    ;;       "org.freedesktop.systemd1.Manager"
    ;;       "UnitNew"
    ;;       (lambda (unit-name unit-path)
    ;;         (with-current-buffer buf
    ;;           (unless systemctl-ui--reloading
    ;;             (message "NEW: %S %S" unit-path unit-name)
    ;;             (systemctl-ui--refresh-unit unit-path)))))
    ;;      systemctl-ui--dbus-subscriptions)

    (push (dbus-register-signal
           bus
           "org.freedesktop.systemd1"
           "/org/freedesktop/systemd1"
           "org.freedesktop.systemd1.Manager"
           "UnitRemoved"
           (systemctl-ui--signal-handler (_ unit-path)
             (with-current-buffer buf
               (unless systemctl-ui--reloading
                 (systemctl-ui--remove-unit unit-path)))))
          systemctl-ui--dbus-subscriptions)

    (push (dbus-register-signal
           bus
           "org.freedesktop.systemd1"
           "/org/freedesktop/systemd1"
           "org.freedesktop.systemd1.Manager"
           "Reloading"
           (systemctl-ui--signal-handler (active)
             (with-current-buffer buf
               (setq systemctl-ui--reloading active)
               (unless systemctl-ui--reloading
                 (systemctl-ui--update-units)))))
          systemctl-ui--dbus-subscriptions)

    (push (dbus-register-signal
           bus
           "org.freedesktop.systemd1"
           "/org/freedesktop/systemd1"
           "org.freedesktop.systemd1.Manager"
           "UnitFilesChanged"
           (systemctl-ui--signal-handler ()
             (with-current-buffer buf
               (unless systemctl-ui--reloading
                 (systemctl-ui--update-units)))))
          systemctl-ui--dbus-subscriptions)

    (push (dbus-register-signal
           bus
           "org.freedesktop.systemd1"
           nil  ;; Match any object path
           "org.freedesktop.DBus.Properties"
           "PropertiesChanged"
           (systemctl-ui--signal-handler
               (_ changed-properties invalidated-properties)
             (with-current-buffer buf
               (systemctl-ui--handle-unit-properties-changed
                (dbus-event-path-name last-input-event)
                changed-properties invalidated-properties)))
           :arg0 "org.freedesktop.systemd1.Unit"
           :path-namespace "/org/freedesktop/systemd1/unit")
          systemctl-ui--dbus-subscriptions)))

(defconst systemctl-ui--unit-properties
  '(("Id" . unit)
    ("ActiveState" . active-state)
    ("SubState" . active-substate)
    ("UnitFileState" . enablement-state)
    ("Description" . description)
    ("FragmentPath" . filename)))

(defun systemctl-ui--handle-unit-properties-changed (unit-path changed-properties invalidated-properties)
  "Handle PropertiesChanged signal for a unit object.

UNIT-PATH is the D-Bus object path of the unit.
CHANGED-PROPERTIES is an alist of changed properties.
INVALIDATED-PROPERTIES is a list of invalidated property names."
  (if-let* ((unit (and (not (and invalidated-properties
                                 (cl-loop for (prop . _) in systemctl-ui--unit-properties
                                          thereis (member prop invalidated-properties))))
                       (cdr (assoc unit-path systemctl-ui--units)))))
      (cl-loop for (prop . key) in systemctl-ui--unit-properties
               for val = (assoc prop changed-properties)
               when val do
               (setf (alist-get key unit) (caadr val))
               finally do (tabulated-list-revert))
    (systemctl-ui--refresh-unit unit-path)))

(defun systemctl-ui--refresh-unit (unit-path)
  "Refresh information for UNIT-PATH."
    (if-let* ((unit (systemctl-ui--fetch-single-unit unit-path)))
        (setf (alist-get unit-path systemctl-ui--units nil nil #'string=)
              unit)
      (setq systemctl-ui--units
            (assoc-delete-all unit-path systemctl-ui--units #'string=)))
    (tabulated-list-revert))

(defun systemctl-ui--remove-unit (unit-path)
  "Remove UNIT-PATH from the units list."
  (setq systemctl-ui--units
        (assoc-delete-all unit-path systemctl-ui--units #'string=))
  (tabulated-list-revert))

(defun systemctl-ui--fetch-single-unit (unit-path)
  "Fetch current information for UNIT-PATH using D-Bus properties."
  (when-let* ((unit-props (dbus-get-all-properties
                           (systemctl--bus-for-manager systemctl-ui--manager)
                           "org.freedesktop.systemd1"
                           unit-path
                           "org.freedesktop.systemd1.Unit")))
    (cons (cons 'dbus-path unit-path)
          (cl-loop for (prop . attr) in systemctl-ui--unit-properties
                   for val = (alist-get prop unit-props nil nil #'string=)
                   when val collect (cons attr val)))))

(defun systemctl-ui--cleanup-dbus ()
  "Clean up D-Bus subscriptions when the buffer is killed."
  (when systemctl-ui--dbus-subscriptions
    ;; Unregister individual signal handlers
    (dolist (subscription systemctl-ui--dbus-subscriptions)
      (condition-case err
          (dbus-unregister-object subscription)
        (error
         (message "Failed to unregister D-Bus subscription: %s" (error-message-string err)))))

    ;; Unsubscribe from systemd events on both buses
    (let ((bus (systemctl--bus-for-manager systemctl-ui--manager)))
      (condition-case err
          (dbus-call-method
           bus
           "org.freedesktop.systemd1"
           "/org/freedesktop/systemd1"
           "org.freedesktop.systemd1.Manager"
           "Unsubscribe")
        (error
         (message "Failed to unsubscribe from %s systemd events: %s" bus (error-message-string err)))))

    (setq systemctl-ui--dbus-subscriptions nil
          systemctl-ui--dbus-initialized nil)))

(defsubst systemctl-ui--buffer-name (manager)
  "Return the buffer name to use when listing MANAGERs units."
  (format "*systemctl (%S)*" manager))

;;;###autoload
(defun systemctl-ui-list-units (&optional manager)
  "Display systemd units for MANAGER in a list."
  (interactive (list (or systemctl-ui-manager
                         (pcase (read-answer
                                 "Display [s]ystem or [u]ser daemon? "
                                 '(("system" ?s "display system units")
                                   ("user" ?u "display user units")
                                   ("quit" ?q "abort")))
                           ("system" 'system)
                           ("user" 'user)
                           (_ (keyboard-quit))))))
  (unless manager (setq manager 'system))
  (let ((buffer (get-buffer-create (systemctl-ui--buffer-name manager))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'systemctl-ui-mode)
        (systemctl-ui-mode)
        (setq systemctl-ui--manager manager)
        (systemctl-ui--setup-dbus)
        (add-hook 'kill-buffer-hook #'systemctl-ui--cleanup-dbus nil t))
      (systemctl-ui--update-units))
    (switch-to-buffer buffer)))

(provide 'systemctl-ui)
;;; systemctl-ui.el ends here
