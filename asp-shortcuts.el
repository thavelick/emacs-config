(defun insert-asp-tag ()
  "Inserts the asp tag <% %>.  Surrounds selected text if mark is set"
  (interactive)
  (let ((before (if (use-region-p) (region-beginning) (point)))
    (after (if (use-region-p) (region-end) (point))))
  (goto-char after)
  (insert " %>")
  (goto-char before)
  (insert "<% ")))
