(defun yxl-ess/insert-pipe ()
  (interactive)
  (insert "%>%"))

(defun yxl-ess/R-hook ())

(defun yxl-ess/ess-hook ()
  (setq-local comment-add 0))

(defun spacemacs/ess-start-repl ()
  "Start a REPL corresponding to the ess-language of the current buffer."
  (interactive)
  (cond
   ((string= "S" ess-language) (call-interactively 'R))
   ((string= "STA" ess-language) (call-interactively 'stata))
   ((string= "SAS" ess-language) (call-interactively 'SAS))
   ((string= "julia" ess-language) (call-interactively 'julia))))

(defun spacemacs//ess-fix-read-only-inferior-ess-mode ()
  "Fixes a bug when `comint-prompt-read-only' in non-nil.
See https://github.com/emacs-ess/ESS/issues/300"
  (setq-local comint-use-prompt-regexp nil)
  (setq-local inhibit-field-text-motion nil))
