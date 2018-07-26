;; -*- lexical-binding: t -*-

(defun yxl-elpy//inferior-python-mode-hook-setup ()
  ;; disable smartscan-mode to make M-p and M-n select
  ;; previous/next statement in python shell
  (when (featurep 'smartscan)
    (smartscan-mode -1))
  ;; for some reason, q is bound to exit even in insert state -> undo this
  (define-key evil-insert-state-local-map "q" 'self-insert-command))

(defun yxl-elpy//elpy-mode-hook-setup ()
  ;; after 2 seconds or C-tab
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.2)
  (define-key elpy-mode-map (kbd "C-<tab>") 'company-complete))

(defun yxl-elpy/insert-codecell-above ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (insert "# <codecell>\n")))

(defun yxl-elpy/insert-markdowncell-above ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (insert "# <markdowncell>\n")))
