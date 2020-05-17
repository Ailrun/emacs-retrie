;;; retrie.el --- Refactoring Haskell code with retrie

;; Copyright (C) 2020 Junyoung Clare Jang

;; Author: Junyoung Clare Jang <jjc9310@gmail.com>
;; Maintainer: Junyoung Clare Jang <jjc9310@gmail.com>
;; Created: 2020/05/16
;; Version: 0.0.1
;; Keywords: files, languages, tools
;; URL: https://github.com/Ailrun/emacs-retrie
;; Package-Requires: ((emacs "24.5"))

;; This program is free software: you can redistribute it and/or modify
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

;; This package is a Emacs wrapper of retrie.
;; Currently this provides the following 4 functions
;; - retrie-unfold-in-current-buffer
;; - retrie-fold-in-current-buffer
;; - retrie-unfold-in-buffer
;; - retrie-fold-in-buffer

;;; Change Log:

;;; Code:
(require 'subr-x)

(defgroup retrie
  nil
  "Settings for Haskell tool retrie"
  :tag "Retrie"
  :group 'convenience
  :group 'haskell
  :group 'tools
  :package-version '(retrie . "0.0.1")
  :prefix "retrie-")

(defcustom retrie-executable "retrie"
  "The name of the retrie executable to find."
  :group 'retrie
  :package-version '(retrie . "0.0.1")
  :type 'string
  :risky t)

(defcustom retrie-use-single-thread nil
  "A boolean to disable parallel executions of retrie (usually for debugging).
Disabling works only when `retrie-log-level' is not loud."
  :group 'retrie
  :package-version '(retrie . "0.0.1")
  :type 'boolean)

(defcustom retrie-log-level 0
  "The level of verbosity used to print retrie log.
Setting loud makes parallel executions of retrie disabled"
  :group 'retrie
  :package-version '(retrie . "0.0.1")
  :type '(choice
          (const :tag "silent" 0)
          (const :tag "normal" 1)
          (const :tag "loud" 0)))

(defcustom retrie-other-arguments nil
  "Other arguments that are not specified as a custom variable in this plugin.
They will be passed to retrie along with the options created from the retrie- custom variables."
  :group 'retrie
  :package-version '(retrie . "0.0.1")
  :type '(list string)
  :risky t)

(defun retrie-unfold-in-current-buffer (name)
  "Unfold the function definition of the name NAME in the current buffer."
  (interactive "sname of function to unfold:")
  (retrie--unfold-in-buffer (current-buffer) name))

(defun retrie-fold-in-current-buffer (name)
  "Fold the function definition of the name NAME in the current buffer."
  (interactive "sname of function to fold:")
  (retrie--fold-in-buffer (current-buffer) name))

(defun retrie-unfold-in-buffer (buffername name)
  "Unfold the function definition of the name NAME in the buffer BUFFERNAME."
  (interactive "*bbuffer:\nsname of function to unfold in %s:")
  (retrie--unfold-in-buffer (get-buffer buffername) name))

(defun retrie-fold-in-buffer (buffername name)
  "Fold the function definition of the name NAME in the buffer BUFFERNAME."
  (interactive "*bbuffer:\nsname of function to fold in %s:")
  (retrie--fold-in-buffer (get-buffer buffername) name))

(defun retrie--unfold-in-buffer (buffer name)
  "Internal function to unfold the function definition of the name NAME in BUFFER."
  (retrie--call-retrie-for-buffer buffer "--unfold" name))

(defun retrie--fold-in-buffer (buffer name)
  "Internal function to fold the function definition of the name NAME in BUFFER."
  (retrie--call-retrie-for-buffer buffer "--fold" name))

(defun retrie--call-retrie-for-buffer (buffer &rest args)
  "Internal function to call retrie in BUFFER with ARGS."
  (let ((filename (buffer-file-name buffer)))
    (unless filename
      (error "%s is not visiting any file.  Abort" (buffer-name buffer)))
    (apply #'retrie--call-retrie "--target-file" filename args)))

(defun retrie--call-retrie (&rest args)
  "Internal function to call retrie with ARGS."
  (let ((logbuf (generate-new-buffer "*retrie*"))
        (args
         (append
          args
          (if retrie-use-single-thread '("--single-threaded") nil)
          (list "--verbosity" (number-to-string retrie-log-level))
          retrie-other-arguments))
        (retrie (executable-find retrie-executable)))
    (unless retrie
      (error "Cannot find %s in exec-path.  Please check your exec-path" retrie-executable))
    (let ((process (apply #'start-process retrie logbuf retrie-executable args)))
      (set-process-sentinel process 'retrie--sentinel))))

(defun retrie--sentinel (process event)
  "Internal function taking PROCESS and EVENT."
  (if (and (= 0 (process-exit-status process))
           (process-live-p process))
      (retrie--print-log "retrie is %s" (string-trim-right event))
    (with-current-buffer (process-buffer process)
      (retrie--print-log "retrie is %s\n%s" (string-trim-right event) (string-trim-right (buffer-string))))
    (kill-buffer (process-buffer process))))

(defun retrie--print-log (string &rest args)
  "Internal function taking STRING and ARGS."
  (apply #'message string args))

(provide 'retrie)
;;; retrie.el ends here
