(defun yxl-prog/evil-wrap-line-f ()
  (interactive)
  (end-of-line)
  (call-interactively #'set-mark-command)
  (back-to-indentation)
  (evil-surround-region (region-beginning) (region-end) t ?f))

(defun yxl-prog/evil-wrap-line-f-print ()
  (interactive)
  (end-of-line)
  (call-interactively #'set-mark-command)
  (back-to-indentation)
  ;; NOTE: this is conditional on ?F is set to `yxl-evil-surround-function-print'
  (evil-surround-region (region-beginning) (region-end) t ?F))
