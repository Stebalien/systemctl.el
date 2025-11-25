;;; systemctl-embark.el --- Embark integration for systemctl  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Steven Allen

;; Author: Steven Allen <steven@stebalien.com>
;; Keywords:

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

(require 'systemctl-ui)

(defvar embark-exporters-alist)

;;;###autoload
(defun systemctl-embark-export (candidates)
  "Embark exporter for systemctl.
Exports CANDIDATES to a `systemctl-ui-mode' buffer."
  (let ((buf (generate-new-buffer "*Embark Export Systemctl*")))
    (with-current-buffer buf
      (systemctl-ui-mode)
      (let (tables)
        (cl-loop
         for candidate in candidates
         for manager = (intern (directory-file-name
                                (file-name-directory candidate)))
         for unit = (file-name-nondirectory candidate)
         for table = (or (alist-get manager tables)
                         (let ((table (make-hash-table :test 'equal)))
                           (push (cons manager (make-hash-table :test 'equal))
                                 tables)
                           table))
         do (puthash unit t table))
        (setq systemctl-ui--units
              (cl-loop
               for (manager . units) in tables
               nconc (cl-loop
                      for unit in (systemctl--list-all-units manager nil)
                      when (gethash (alist-get 'unit unit) units)
                      collect unit))))
      (tabulated-list-revert))
    (pop-to-buffer buf)))

;;;###autoload
(with-eval-after-load 'embark
  (setf (alist-get 'systemctl embark-exporters-alist) #'systemctl-embark-export))

(provide 'systemctl-embark)
;;; systemctl-embark.el ends here
