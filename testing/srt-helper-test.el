;;; srt-helper-test.el --- Test the srt-helper major mode.

;; Copyright (C) 2014  Yosuke Funahashi <yosuke@funahashi.cc>

;; Author: Yosuke Funahashi <yosuke@funahashi.cc>

;; This software is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Test Development
;; For test development purposes a number of navigation and test
;; function construction routines are available as a git submodule
;; (jump.el)
;; Install with...
;; $ git submodule init
;; $ git submodule update

;;;; Code:

;;; Constants

(unless (and (boundp 'srt-helper-batch-test) srt-helper-batch-test)
  (let* ((srt-helper-test-dir (expand-file-name
			       (file-name-directory
				(or load-file-name buffer-file-name))))
	 (srt-helper-lisp-dir (expand-file-name
			       (concat srt-helper-test-dir "../lisp"))))
    (unless (featurep 'srt-helper)
      (setq load-path (cons srt-helper-lisp-dir load-path))
      (require 'srt-helper)
    (let* ((load-path (cons
		       srt-helper-test-dir
		       (cons
			(expand-file-name "jump" srt-helper-test-dir)
			load-path))))
      (require 'cl)
      (require 'ert)
      (require 'ert-x)
      (when (file-exists-p
	     (expand-file-name "jump/jump.el" srt-helper-test-dir))
	(require 'jump)
	(require 'which-func)))))

(defconst srt-helper-test-default-test-file-name "tests.el"
  "For each defun a separate file with tests may be defined.
tests.el is the fallback or default if you like.")

(defconst srt-helper-test-default-directory-name "testing"
  "Basename or the directory where the tests live.
srt-helper-test searches this directory up the directory tree.")

(defconst srt-helper-test-dir
  (expand-file-name (file-name-directory (or load-file-name buffer-file-name))))

(defconst srt-helper-base-dir
  (expand-file-name ".." srt-helper-test-dir))

(defconst srt-helper-test-example-dir
  (expand-file-name "examples" srt-helper-test-dir))

(defconst srt-helper-test-file
  (expand-file-name "normal.srt" srt-helper-test-example-dir))


;;; Functions for writing tests
(put 'missing-test-dependency
     'error-conditions
     '(error missing-test-dependency))

(defmacro srt-helper-test-in-example-file (file &rest body)
  "Execute body in the srt-helper-mode example file."
  (declare (indent 1))
  `(let* ((my-file (or ,file srt-helper-test-file))
	  (visited-p (get-file-buffer my-file))
	  to-be-removed)
     (save-window-excursion
       (save-match-data
	 (find-file my-file)
	 (unless (eq major-mode 'srt-helper-mode)
	   (srt-helper-mode))
	 (setq to-be-removed (current-buffer))
	 (goto-char (point-min))
	 (save-restriction ,@body)))
     (unless visited-p
       (kill-buffer to-be-removed))))
(def-edebug-spec srt-helper-test-in-example-file (form body))

(defmacro srt-helper-test-at-marker (file marker &rest body)
  "Run body after placing the point at MARKER in FILE.
Note the uuidgen command-line command can be useful for
generating unique markers for insertion as anchors into srt
files."
  (declare (indent 2))
  `(srt-helper-test-in-example-file ,file
     (goto-char (point-min))
     (re-search-forward (regexp-quote ,marker))
     ,@body))
(def-edebug-spec srt-helper-test-at-marker (form form body))

(defmacro srt-helper-test-with-temp-text (text &rest body)
  "Run body in a temporary buffer with srt-helper-mode as the active
mode holding TEXT.  If the string \"<point>\" appears in TEXT
then remove it and place the point there before running BODY,
otherwise place the point at the beginning of the inserted text."
  (declare (indent 1))
  `(let ((inside-text (if (stringp ,text) ,text (eval ,text))))
     (with-temp-buffer
       (org-mode)
       (let ((point (string-match "<point>" inside-text)))
	 (if point
	     (progn
	       (insert (replace-match "" nil nil inside-text))
	       (goto-char (1+ (match-beginning 0))))
	   (insert inside-text)
	   (goto-char (point-min))))
       ,@body)))
(def-edebug-spec srt-helper-test-with-temp-text (form body))

(defmacro srt-helpr-test-with-temp-text-in-file (text &rest body)
  "Run body in a temporary file buffer with srt-helper-mode as the active mode."
  (declare (indent 1))
  (let ((results (gensym)))
    `(let ((file (make-temp-file "srt-helper-test"))
	   (kill-buffer-query-functions nil)
	   (inside-text (if (stringp ,text) ,text (eval ,text)))
	   ,results)
       (with-temp-file file (insert inside-text))
       (find-file file)
       (srt-helper-mode)
       (setq ,results (progn ,@body))
       (save-buffer) (kill-buffer (current-buffer))
       (delete-file file)
       ,results)))
(def-edebug-spec srt-helper-test-with-temp-text-in-file (form body))


;;; Navigation Functions
(when (featurep 'jump)
  (defjump srt-helper-test-jump
    (("lisp/\\1.el" . "testing/lisp/test-\\1.el")
     ("lisp/\\1.el" . "testing/lisp/\\1.el/test.*.el")
     ("testing/lisp/test-\\1.el" . "lisp/\\1.el")
     ("testing/lisp/\\1.el" . "lisp/\\1.el/test.*.el"))
    (concat srt-helper-base-dir "/")
    "Jump between srt-helpr-mode files and their tests."
    (lambda (path)
      (let* ((full-path (expand-file-name path srt-helper-base-dir))
	     (file-name (file-name-nondirectory path))
	     (name (file-name-sans-extension file-name)))
	(find-file full-path)
	(insert
	 ";;; " file-name "\n\n"
	 ";; Copyright (c) " (nth 5 (decode-time (current-time)))
	 " " user-full-name "\n"
	 ";; Authors: " user-full-name "\n\n"
	 ";; Released under the GNU General Public License version 3\n"
	 ";; see: http://www.gnu.org/licenses/gpl-3.0.html\n\n"
	 ";;;; Comments:\n\n"
	 ";; Template test file for srt-helper-mode tests\n\n"
	 "\n"
	 ";;; Code:\n"
	 "(let ((load-path (cons (expand-file-name\n"
	 "			\"..\" (file-name-directory\n"
	 "			      (or load-file-name buffer-file-name)))\n"
	 "		       load-path)))\n"
	 "  (require 'srt-helper-test)\n\n"
	 "\n"
	 ";;; Tests\n"
	 "(ert-deftest " name "/example-test ()\n"
	 "  \"Just an example to get you started.\"\n"
	 "  (should t)\n"
	 "  (should-not nil)\n"
	 "  (should-error (error \"errr...\")))\n\n\n"
	 "(provide '" name ")\n\n"
	 ";;; " file-name " ends here\n") full-path))
    (lambda () ((lambda (res) (if (listp res) (car res) res)) (which-function)))))

(define-key emacs-lisp-mode-map "\M-\C-j" 'srt-helper-test-jump)


;;; Miscellaneous helper functions
(defun srt-helper-test-strip-text-props (s)
  "Return S without any text properties."
  (let ((noprop (copy-sequence s)))
    (set-text-properties 0 (length noprop) nil noprop)
    noprop))


(defun srt-helper-test-string-exact-match (regex string &optional start)
  "case sensative string-match"
  (let ((case-fold-search nil)
        (case-replace nil))
    (if(and (equal regex "")
	    (not(equal string "")))
        nil
      (if (equal 0 (string-match regex string start))
          t
        nil))))

;;; Load and Run tests
(defun srt-helper-test-load ()
  "Load up the srt-helper-mode test suite."
  (interactive)
  (flet ((rld (base)
	      ;; Recursively load all files, if files throw errors
	      ;; then silently ignore the error and continue to the
	      ;; next file.  This allows files to error out if
	      ;; required executables aren't available.
	      (mapc
	       (lambda (path)
		 (if (file-directory-p path)
		     (rld path)
		   (condition-case err
		       (when (string-match "^[A-Za-z].*\\.el$"
					   (file-name-nondirectory path))
			 (load-file path))
		     (missing-test-dependency
		      (let ((name (intern
				   (concat "org-missing-dependency/"
					   (file-name-nondirectory
					    (file-name-sans-extension path))))))
			(eval `(ert-deftest ,name ()
				 :expected-result :failed (should nil))))))))
	       (directory-files base 'full
				"^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*\\.el$"))))
    (rld (expand-file-name "lisp" org-test-dir))))

(defun srt-helper-test-current-defun ()
  "Test the current function."
  (interactive)
  (ert (which-function)))

(defun srt-helper-test-current-file ()
  "Run all tests for current file."
  (interactive)
  (ert (concat "test-"
	       (file-name-sans-extension
		(file-name-nondirectory (buffer-file-name)))
	       "/")))

(defvar srt-helper-test-buffers nil
  "Hold buffers open for running srt-helper-mode tests.")

(defun srt-helper-test-touch-all-examples ()
  (dolist (file (directory-files
		 srt-helper-test-example-dir 'full
		 "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*\\.srt$"))
    (unless (get-file-buffer file)
      (add-to-list 'org-test-buffers (find-file file)))))

(defun srt-helper-test-kill-all-examples ()
  (while srt-helper-test-buffers
    (let ((b (pop srt-helper-test-buffers)))
      (when (buffer-live-p b) (kill-buffer b)))))

(defun srt-helper-test-run-batch-tests (&optional srt-helper-test-selector)
  "Run all tests matching an optional regex which defaults to \"\\(srt\\)\".
Load all test files first."
  (interactive)
  (let ((srt-helper-id-track-globally t)
	(srt-helper-test-selector
	 (if srt-helper-test-selector srt-helper-test-selector "\\(srt\\)"))
	srt-helper-confirm-babel-evaluate srt-helper-startup-folded vc-handled-backends)
    (srt-helper-test-touch-all-examples)
    (srt-helper-test-update-id-locations)
    (srt-helper-test-load)
    (message "selected tests: %s" srt-helper-test-selector)
    (ert-run-tests-batch-and-exit srt-helper-test-selector)))

(defun srt-helper-test-run-all-tests ()
  "Run all defined tests matching \"\\(srt\\)\".
Load all test files first."
  (interactive)
  (srt-helper-test-touch-all-examples)
  (srt-helper-test-update-id-locations)
  (srt-helper-test-load)
  (ert "\\(srt\\)")
  (srt-helper-test-kill-all-examples))

(provide 'srt-helper-test)

;;; srt-helper-test.el ends here
