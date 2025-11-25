;;; systemctl-status.el --- Systemctl status buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Steven Allen

;; Author: Steven Allen <steven@stebalien.com>
;; Keywords: unix

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'term)
(require 'systemctl)

(defvar-local systemctl-status--killing-indirect-buffer nil
  "Non-nil when killing the indirect systemctl buffer.
Prevents recursive buffer killing.")

(defun systemctl-status--kill-base ()
  "Kill the base buffer of a systemctl status buffer."
  (when-let* ((_(not systemctl-status--killing-indirect-buffer))
              (base (buffer-base-buffer)))
    (let ((systemctl-status--killing-indirect-buffer t))
      (kill-buffer base))))

(define-derived-mode systemctl-status-mode special-mode "Systemctl Status"
  "Major mode for viewing systemd unit status."
  (add-hook 'kill-buffer-hook #'systemctl-status--kill-base nil t)
  (setq truncate-lines t))

(defun systemctl-status--term-sentinel (proc msg)
  "Replacement `term' sentinel for `systemctl-status'.
PROC and MSG are the same as in `term-sentinel'.

This function calls `term-sentinel', but then strips the process status
message from the end of the buffer."
  (let ((end (point-max))
        (buf (process-buffer proc)))
    (term-sentinel proc msg)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (delete-region end (point-max))))))

;;;###autoload
(defun systemctl-status (unit &optional manager)
  "Show detailed information about the UNIT belonging to MANAGER."
  (interactive (let-alist (systemctl-read-unit "Status: ")
                 (list .unit .manager)))
  (unless manager (setq manager 'system))
  (let* ((term-name (format "systemctl (%s): %s" manager unit))
         (term-buffer (get-buffer-create
                       (format " *term-%s*" term-name)
                       t))
         (buffer-name (format "*%s*" term-name)))
    (unless (term-check-proc term-buffer)
      (with-current-buffer term-buffer
        (erase-buffer)
        (term-mode)
        (setq-local term-width #xffff))
      (let ((process-environment
             (append
              '("SYSTEMD_COLORS=1" "SYSTEMD_PAGER=")
              process-environment))
            (args (list "status" "-l" "--" unit)))
        (when (eq manager 'user) (push "--user" args))
        (term-exec term-buffer term-name "systemctl" nil args))
      (let ((proc (get-buffer-process term-buffer)))
        ;; Disable query-on-exit.
        (set-process-query-on-exit-flag proc nil)
        ;; Strip the process status from the buffer.
        (set-process-sentinel proc #'systemctl-status--term-sentinel)))
    (let ((buffer (or (get-buffer buffer-name)
                      (make-indirect-buffer term-buffer buffer-name nil))))
      (with-current-buffer buffer
        (systemctl-status-mode))
      (pop-to-buffer buffer))))

(provide 'systemctl-status)
;;; systemctl-status.el ends here
