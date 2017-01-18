(defun yxl-utils/home ()
  "Force recreation of the spacemacs buffer.

Accepts C-u arg to delete all other windows."
  (interactive)
  (when (equal current-prefix-arg '(4))
    (delete-other-windows))
  (setq spacemacs-buffer--last-width nil)
  (spacemacs-buffer/goto-buffer t))
