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
