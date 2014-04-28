srt-helper-mode
===============

Srt-helper (srt-helper.el) is a major mode for adjusting time offsets of subtitles in the srt format.

This tool shifts all timestamps of a subtitle file to synchronize the subtitles to a video when there is a slight offset between the two (this can be the case when the subtitles and the video come from two different sources).

Installation
------------

To use srt-helper.el, put it in your load-path and add the following to your .emacs

```
(require 'srt-helper)
```

 or

```
(autoload 'srt-helper-mode "srt-helper" "srt helper major mode." t)
```

Customization:
--------------

You can use your own evaluation function for heuristic serarhes to find an srt cue close to a time point. The default function is:

```
(defvar srt-helper-evaluation-fn
  (lexical-let (log)
	(lambda (ms timings &optional init)
	  (if init
	      (setq log nil)
	    (unless timings
	      (throw 'close nil))
	    (let ((diff (- (oref timings :start)
			   ms)))
	    (if log
		(cond ((= diff 0) (progn
				    (goto-char (point))
				    (beginning-of-line)
				    (throw 'close nil)))
		      ((< (* diff (car (nth 0 log))) 0)
		       (if (<= (abs diff)
			       (abs (car (nth 0 log))))
			   (progn
			     (goto-char (point))
			     (beginning-of-line)
			     (throw 'close nil))
			 (goto-char (cdr (nth 0 log)))
			 (beginning-of-line)
			 (throw 'close nil)))))
	    (push (cons diff (point)) log)))))
  "An evaluation function to find an srt cue close to
a time offset of MS.")
```

Usage notes:
------------

Useful srt-helper-specific editing features:

* Offset all srt cues by a certain extent with 'C-c o'. You can use various formats of time to designate the delay: -3.14, 1:32,572, 02:41:56.3, 1560, etc.

* Offset all srt cues as to make current cue's start time into a different time with 'C-c t'

* Search forward or backward from current point for an srt cue which seems close to a time offset represented by a string representing a time offset with 'C-c f' or 'C-c b'.

