
(global-set-key (kbd "<backtab>") 'indent-back)
(global-set-key (kbd "C-<tab>") 'indent-forward)

(defun classic-indent (width)
  "Tab the current line or block the 'classic' way"
  (save-excursion
  (if (not (use-region-p)) (select-current-line))
  (indent-rigidly (mark) (point) width)))

(defun indent-forward ()
  "tab two space forward"
  (interactive)
  (classic-indent 2))

(defun indent-back ()
  "tab two spaces back"
  (interactive)
  (classic-indent -2))


(defun select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))


