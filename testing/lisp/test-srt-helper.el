
(ert-deftest srt-helper-test-basic ()
    (srt-helper-test-in-example-file (expand-file-name "~/dev/srt-helper-mode/testing/examples/short-sample.1.srt")
      (goto-char (point-min))
      (srt-helper-search-forward "14")
      (should (equal (buffer-substring-no-properties (point) (+ (point) 12))
		     "00:00:14,812"))))

(ert-deftest srt-helper-test-search-mix ()
  (srt-helper-test-in-example-file (expand-file-name "~/dev/srt-helper-mode/testing/examples/Romeo and Juliet (1936).1.srt")
      (goto-char (point-min))
      (srt-helper-search-forward "14:00")
      (should (equal (buffer-substring-no-properties (point) (+ (point) 12))
		     "00:13:58,000"))
      (srt-helper-search-forward "23:34")
      (should (equal (buffer-substring-no-properties (point) (+ (point) 12))
		     "00:23:34,600"))
      ))






