;;; unzip-test.el --- Test unzip -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

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

;;; Code:

(require 'ert)
(require 'unzip)

(when noninteractive
  (transient-mark-mode))

(defvar unzip-test-sample-zip-url "https://github.com/twlz0ne/unzip.el/archive/master.zip")
(defvar unzip-test-sample-zip-file (expand-file-name "unzip.el-master.zip"))
(defvar unzip-test-output-dir "/tmp/emacs-unzip-test")

(cl-defun unzip-sync-test (from to &key strip-component overwrite &allow-other-keys)
  (ignore-errors
    (delete-directory to t))
  (mkdir unzip-test-output-dir t)
  (unzip from
         to
         :strip-component strip-component
         :overwrite       overwrite
         :sync-finish-fn nil))

(cl-defun unzip-async-test (from to &key strip-component overwrite async-finish-fn &allow-other-keys)
  (let ((async-finished-p nil)
        (async-result nil))
    (ignore-errors
      (delete-directory to t))
    (mkdir unzip-test-output-dir t)
    (unzip from
           to
           :strip-component strip-component
           :overwrite       overwrite
           :async-finish-fn (lambda (result)
                              (setq async-result result)
                              (setq async-finished-p t)))
    (while (not async-finished-p)
      (sit-for 1))
    (when (functionp async-finish-fn)
      (funcall async-finish-fn async-result))))

(unless (file-exists-p unzip-test-sample-zip-file)
  (unzip--copy-file unzip-test-sample-zip-url unzip-test-sample-zip-file))

;;; local file

(ert-deftest unzip-test-localfile-sync-0 ()
  (unzip-sync-test
   unzip-test-sample-zip-file
   unzip-test-output-dir
   :strip-component 0
   :overwrite t)
  (should
   (and (file-exists-p (concat unzip-test-output-dir "/unzip.el-master/README.md"))
        (file-exists-p (concat unzip-test-output-dir "/unzip.el-master/LICENSE")))))

(ert-deftest unzip-test-localfile-sync-1 ()
  (unzip-sync-test
   unzip-test-sample-zip-file
   unzip-test-output-dir
   :strip-component 1
   :overwrite t)
  (should
   (and (file-exists-p (concat unzip-test-output-dir "/README.md"))
           (file-exists-p (concat unzip-test-output-dir "/LICENSE")))))

(ert-deftest unzip-test-localfile-async-0 ()
  (unzip-async-test
   unzip-test-sample-zip-file
   unzip-test-output-dir
   :strip-component 0
   :overwrite t
   :async-finish-fn
   (lambda (result)
     (should
      (and (file-exists-p (concat unzip-test-output-dir "/unzip.el-master/README.md"))
           (file-exists-p (concat unzip-test-output-dir "/unzip.el-master/LICENSE")))))))

(ert-deftest unzip-test-localfile-async-1 ()
  (unzip-async-test
   unzip-test-sample-zip-file
   unzip-test-output-dir
   :strip-component 1
   :overwrite t
   :async-finish-fn
   (lambda (result)
     (should
      (and (file-exists-p (concat unzip-test-output-dir "/README.md"))
           (file-exists-p (concat unzip-test-output-dir "/LICENSE")))))))

;;; url

(ert-deftest unzip-test-url-sync-0 ()
  (unzip-sync-test
   unzip-test-sample-zip-url
   unzip-test-output-dir
   :strip-component 0
   :overwrite t)
  (should
   (and (file-exists-p (concat unzip-test-output-dir "/unzip.el-master/README.md"))
        (file-exists-p (concat unzip-test-output-dir "/unzip.el-master/LICENSE")))))

(ert-deftest unzip-test-url-sync-1 ()
  (unzip-sync-test
   unzip-test-sample-zip-url
   unzip-test-output-dir
   :strip-component 1
   :overwrite t)
  (should
   (and (file-exists-p (concat unzip-test-output-dir "/README.md"))
           (file-exists-p (concat unzip-test-output-dir "/LICENSE")))))

(ert-deftest unzip-test-url-async-0 ()
  (unzip-async-test
   unzip-test-sample-zip-url
   unzip-test-output-dir
   :strip-component 0
   :overwrite t
   :async-finish-fn
   (lambda (result)
     (should
      (and (file-exists-p (concat unzip-test-output-dir "/unzip.el-master/README.md"))
           (file-exists-p (concat unzip-test-output-dir "/unzip.el-master/LICENSE")))))))

(ert-deftest unzip-test-url-async-1 ()
  (unzip-async-test
   unzip-test-sample-zip-url
   unzip-test-output-dir
   :strip-component 1
   :overwrite t
   :async-finish-fn
   (lambda (result)
     (should
      (and (file-exists-p (concat unzip-test-output-dir "/README.md"))
           (file-exists-p (concat unzip-test-output-dir "/LICENSE")))))))

(cl-defun func (&rest body
                      &key foo
                      bar))

(provide 'unzip-test)

;;; unzip-test.el ends here
