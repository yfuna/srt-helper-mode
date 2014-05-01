;;; srt-helper.el --- Srt Helper Major Mode for Emacs

;; Copyright (C) 2014
;; Yosuke Funahashi <yosuke@funahashi.cc>

;; Author: Yosuke Funahashi <yosuke@funahashi.cc>
;; Keywords: SubRip, srt, multimedia, subtitles

;; This file is *NOT* part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Srt-helper (srt-helper.el) is a major mode for adjusting time
;; offsets of subtitles in the srt format.

;; This tool shifts all timestamps of a subtitle file to synchronize
;; the subtitles to a video when there is a slight offset between
;; the two (this can be the case when the subtitles and the video
;; come from two different sources).

;;; Customization:

;; You can use your own evaluation function for heuristic serarhes
;; to find an srt cue close to a time point. The default function
;; is as defined for srt-helper-evaluation-fn.

;;; Usage notes:

;; Useful srt-helper-specific editing features:

;; * Offset all srt cues by a certain extent with 'C-c o'. You can use
;;   various formats of time to designate the delay: -3.14, 1:32,572,
;;   02:41:56.3, 1560, etc.
;;
;; * Offset all srt cues as to make current cue's start time into a
;;   different time with 'C-c t'
;;
;; * Search forward or backward from current point for an srt cue which
;;   seems close to a time offset represented by a string representing a
;;   time offset with 'C-c f' or 'C-c b'.

;;; Installation:

;;
;;   To use srt-helper.el, put it in your load-path and add
;;   the following to your .emacs
;;
;;   (require 'srt-helper)
;;
;; or
;;
;;   (autoload 'srt-helper-mode "srt-helper" "srt helper major mode." t)
;;

;;; Code:

;;;; Dependencies and setup:

(eval-when-compile
  (require 'cl))

(require 'eieio)

(defconst srt-helper-version "0.1.1")

;;;; Major Mode Definition:

(defvar srt-helper-mode-map nil
  "Keymap for srt helper mode.")

(if srt-helper-mode-map 
  nil
  (setq srt-helper-mode-map (make-sparse-keymap))
  (define-key srt-helper-mode-map (kbd "C-c o") 'srt-helper-offset-cues)
  (define-key srt-helper-mode-map (kbd "C-c t") 'srt-helper-offset-cues-towards)
  (define-key srt-helper-mode-map (kbd "C-c s") 'srt-helper-search-forward)
  (define-key srt-helper-mode-map (kbd "C-c r") 'srt-helper-search-backward))

(defvar srt-helper-mode-hook nil
  "*List of functions to call when entering srt helper mode")

(defvar srt-helper-font-lock-keywords
  (list
   '("^0[0-9]:[0-5][0-9]:[0-5][0-9],[0-9][0-9][0-9]" . font-lock-function-name-face)
   '(" -->" . font-lock-doc-face)
   '(" 0[0-9]:[0-5][0-9]:[0-5][0-9],[0-9][0-9][0-9]" . font-lock-constant-face)
   '("^[0-9]+$" . font-lock-comment-face))
  "Font lock keywords for srt helper mode.")

;;;;####autoload
(defun srt-helper-mode ()
  "Major mode for helping adjust srt files.
Special commands:
\\{srt-helper-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map srt-helper-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(srt-helper-font-lock-keywords))
  (setq major-mode 'srt-helper-mode)
  (setq mode-name "srt-helper")
  (run-hooks 'srt-helper-mode-hook))


;;;; Mode Meats:

(defconst srt-helper-timestamp-regexp
  "\\(0[0-9]:[0-5][0-9]:[0-5][0-9],[0-9][0-9][0-9]\\)"
  "A regular expression for an srt timestamp, or the French
timecode format. An srt timestamp is a representations of
a start time offset or end time offset of an srt cue.")

(defconst srt-helper-line2-regexp
  (concat "^"
	  srt-helper-timestamp-regexp
	  " --> "
	  srt-helper-timestamp-regexp
	  "$")
  "A regular expression for timings of an srt cue.")

(defclass srt-helper-timings ()
  ((start :initarg :start
	  :initform 0
	  :type number
	  :documentation "The start time offset in milliseconds.")
   (end :initarg :end
	:initform 0
	:type number
	:documentation "The end time offset in miliseconds."))
  "Class that represents srt timings: a start time offset and
end time offset of an srt cue.")

(defvar srt-helper-evaluation-fn
  (lexical-let (log)
    (lambda (ms timings &optional init)
      (if init
	  (setq log nil)
	(unless timings
	  (throw 'close nil))
	(let ((diff (- (oref timings :start) ms)))
	  (if log
	      (cond ((= diff 0) (progn
				  (goto-char (point))
				  (beginning-of-line)
				  (throw 'close nil)))
		    ((< (* diff (car (first log))) 0)
		     (if (<= (abs diff)
			     (abs (car (first log))))
			 (progn
			   (goto-char (point))
			   (beginning-of-line)
			   (throw 'close nil))
		       (goto-char (cdr (first log)))
		       (beginning-of-line)
		       (throw 'close nil)))))
	  (push (cons diff (point)) log)))))
  "An evaluation function to find an srt cue close to
a time offset of MS.")

(defun srt-helper-search-forward (time-string)
  "Search forward from current point for an srt cue
which seems close to a time offset represented by TIME-STRING."
  (interactive "sGo to around: ")
  (let ((time (srt-helper--maybe-time time-string)))
    (unless (re-search-backward "^[ \t\n]*$" nil t)
      (goto-char (point-min)))
    (funcall srt-helper-evaluation-fn 0 nil t)
    (catch 'close
      (while t
	(funcall srt-helper-evaluation-fn
		 time
		 (srt-helper--get-next-timings))))))

(defun srt-helper-search-backward (time-string)
  "Search backward from current point for an srt cue
which seems close to a time offset represented by TIME-STRING."
  (interactive "sGo to around: ")
  (let ((time (srt-helper--maybe-time time-string)))
    (unless (re-search-forward "^[ \t\n]*$" nil t)
      (goto-char (point-max)))
    (funcall srt-helper-evaluation-fn 0 nil t)
    (catch 'close
      (while t
	(funcall srt-helper-evaluation-fn
		 time
		 (srt-helper--get-previous-timings))))))

(defun srt-helper--maybe-time (string)
  "Interpret and convert STRING into miliseconds
on a best effort basis."
  (if (string-match ":" string)
	(srt-helper--interpret-maybe-timestamp string)
    (truncate
     (* (string-to-number
	 (replace-regexp-in-string "," "." string))
	1000))))
;; (srt-helper--maybe-time "4:16")244000 (#o734440, #x3b920, ?𻤠)
;; (srt-helper--timestamp-to-ms "00:04:16,000")256000

(defun srt-helper--interpret-maybe-timestamp (string)
  "Interpret and convert STRING, which contains colon(s),
into miliseconds on a best effort basis."
  (if (string-match srt-helper-timestamp-regexp string)
      (srt-helper--timestamp-to-ms string)
    (let* ((digits-list (split-string string ":"))
	   (len (length digits-list))
	   (h 0) (m 0) (s 0))
      (cond
       ((= len 2)
	(setq m (string-to-number
		 (first digits-list))
	      s (string-to-number
		 (replace-regexp-in-string "," "." (second digits-list)))))
       ((= len 3)
	(setq h (string-to-number
		 (first digits-list))
	      m (string-to-number
		 (second digits-list))
	      s (string-to-number
		 (replace-regexp-in-string "," "." (third digits-list))))))
      (truncate (* (+ (* (+ (* h 60) m) 60) s) 1000)))))

(defun srt-helper--create-timings-forward ()
  "Return a timings object of an SRT cue
found by searching forward."
  (if (re-search-forward
       srt-helper-line2-regexp nil t)
      (make-instance 'srt-helper-timings
		 :start (srt-helper--timestamp-to-ms (match-string 1))
		 :end (srt-helper--timestamp-to-ms (match-string 2)))))

(defun srt-helper--create-timings-backward ()
  "Return a timings object of an srt cue
found by searching backward."
  (if (re-search-backward
       srt-helper-line2-regexp nil t)
      (make-instance 'srt-helper-timings
		 :start (srt-helper--timestamp-to-ms (match-string 1))
		 :end (srt-helper--timestamp-to-ms (match-string 2)))))

(defun srt-helper--get-current-timings ()
  "Return an srt timings object of the srt cue
on which the point currently is."
  (unless (re-search-backward "^[ \t\n]*$" nil t)
    (goto-char (point-min)))
  (srt-helper--create-timings-forward))

(defun srt-helper--get-next-timings ()
  "Find the next srt timings and return them
as an srt timings object."
  (if (re-search-forward "^[ \t\n]*$" nil t)
      (srt-helper--create-timings-forward)))

(defun srt-helper--get-previous-timings ()
  "Find the previous srt timings and return them
as an srt timings object."
  (if (re-search-backward "^[ \t\n]*$" nil t)
      (srt-helper--create-timings-backward)))

(defun srt-helper--timestamp-to-ms (timestamp)
  "Convert an srt TIMESTAMP to milliseconds"
  (let ((HH (string-to-number (substring timestamp 0 2)))
	(MM (string-to-number (substring timestamp 3 5)))
	(SS (string-to-number (substring timestamp 6 8)))
	(ms (string-to-number (substring timestamp 9 12))))
    (+ (* (+ (* (+ (* HH 60) MM) 60) SS) 1000) ms)))

(defun srt-helper--ms-to-timestamp (ms)
  "Convert TIME in milliseconds to an srt timestamp."
  (let ((HH  (/ ms 3600000))
	(MM  (% (/ ms 60000) 60))
	(SS  (% (/ ms 1000) 60))
	(mmm (% ms 1000)))
    (format "%02d:%02d:%02d,%03d" HH MM SS mmm)))

(defun srt-helper--replace-timestamp (start end)
  "Replace last matched timings by new values:
COUNTER, START, and END."
  (replace-match (format "%s --> %s"
			 (srt-helper--ms-to-timestamp start)
			 (srt-helper--ms-to-timestamp end)) t t))

(defun srt-helper-offset-cues (string)
  "Offset all srt cures by STRING. STRING should be an expression
of time such as -3.14, 1:32,572, 182."
  (interactive "sOffset (-3.14, 1:32,572, 182, etc): ")
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (let ((ms (srt-helper--maybe-time string))
	    (timings (srt-helper--get-current-timings)))
	(srt-helper--replace-timestamp
	   (+ (oref timings :start) ms)
	   (+ (oref timings :end) ms))
	(while (setq timings (srt-helper--get-next-timings))
	  (srt-helper--replace-timestamp
	   (+ (oref timings :start) ms)
	   (+ (oref timings :end) ms)))))))

(defun srt-helper-offset-cues (string)
  "Offset all srt cues by STRING. STRING should be an expression
of time such as -3.14, 1:32,572, 182."
  (interactive "sOffset (-3.14, 1:32,572, 182, etc): ")
  (save-excursion
    (save-match-data
      (srt-helper--offset-cues-by-ms
       (srt-helper--maybe-time string)))))

(defun srt-helper--offset-cues-by-ms (ms)
  "Offset all srt cures by MS."
  (goto-char (point-min))
  (let ((timings (srt-helper--get-current-timings)))
    (srt-helper--replace-timestamp
     (+ (oref timings :start) ms)
     (+ (oref timings :end) ms))
    (while (setq timings (srt-helper--get-next-timings))
      (srt-helper--replace-timestamp
       (+ (oref timings :start) ms)
       (+ (oref timings :end) ms)))))

(defun srt-helper-offset-cues-towards (timestamp)
  "Offset all srt cues as to make current cue's
timestamp to TIMESTAMP."
  (interactive
      (list (read-from-minibuffer "New timestamp: "
				  (srt-helper--ms-to-timestamp
				   (oref (srt-helper--get-current-timings) :start)))))
  (save-excursion
    (save-match-data
      (srt-helper--offset-cues-by-ms
   (- (srt-helper--timestamp-to-ms timestamp)
      (oref (srt-helper--get-current-timings) :start))))))

(provide 'srt-helper)
