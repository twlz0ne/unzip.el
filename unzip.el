;;; unzip.el --- Unzip Wrapper -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2019/06/30
;; Version: 0.0.9
;; Package-Requires: ((emacs "24.4") (async "1.9"))
;; URL: https://github.com/twlz0ne/unzip.el
;; Keywords: tools, archive

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

;; Wrapper of unzip

;;; Change Log:

;;  0.0.9  2019/06/30  Initial version.

;;; Code:

(require 'cl-lib)
(require 'async)

(defun unzip--copy-commands (from to)
  (cond
   ((executable-find "cp")
    (format "bash -c 'mkdir -p %s && cp -R %s %s'"
            to from to))
   ((executable-find "powershell")
    (error "No implement!"))
   (t (error "No copy tool found!"))))

(defun unzip--unzip-commands (from to)
  (cond
   ((executable-find "unzip")
    (format "bash -c 'mkdir -p %s && unzip -qq %s -d %s'"
            to from to))
   ((executable-find "powershell")
    (format "powershell -noprofile -noninteractive -nologo -ex bypass Expand-Archive -path '%s' -dest '%s'"
            from to))
   (t (error "No unzip tool found!"))))

(defun unzip--copy-file (from &optional to)
  (let ((to (or to (make-temp-file "temp-file--" nil ".zip"))))
    (url-copy-file from to 'overwrite)
    to))

(defun unzip-1 (from to &optional strip-component overwrite)
  (let ((from-file
         (if (string-match-p "^https?://" from)
             (unzip--copy-file from)
           from)))
    (if (and strip-component (> strip-component 0))
        (let ((temp-dir (make-temp-file "temp-dir--" t "/")))
          (shell-command (unzip--unzip-commands from-file temp-dir))
          (shell-command (unzip--copy-commands
                           (concat temp-dir (apply 'concat (make-list (1+ strip-component) "/*")))
                           to)))
      (shell-command (unzip--unzip-commands from-file to)))))

(cl-defun unzip (from to &key strip-component overwrite async-finish-fn &allow-other-keys)
  "Unzip file file or url FROM to directory TO.

If STRIP-COMPONENT greater than 0, remove the specified number of leading path elements.
If OVERWRITE not nil, overwrite output dir.
If async-finish-fn not nil, start async and call back when finishes (or 
just give a `t' if there is nothing todo.)
"
  (when (and (file-exists-p to) (not overwrite))
    (error (format "Target file '%s' already exists!" to)))
  (if async-finish-fn
      (async-start
       `(lambda ()
          ,@(mapcar (lambda (sym) `(fset ',sym #',(symbol-function sym)))
                    '(unzip--copy-commands
                      unzip--unzip-commands
                      shell-command-async
                      shell-command
                      unzip--copy-file
                      unzip-1))
          (unzip-1 ,from ,to ,strip-component ,overwrite))
       (when (functionp async-finish-fn)
         async-finish-fn))
    (unzip-1 from to strip-component overwrite)))

(provide 'unzip)

;;; unzip.el ends here
