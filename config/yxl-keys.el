;; mac: use super+3 as "#"
(global-set-key (kbd "s-3") (lambda () (interactive) (insert "#")))
;; (global-set-key (kbd "C-S-p") #'helm-M-x)
(global-set-key (kbd "C-S-p") #'counsel-M-x)
(global-set-key (kbd "C-h") #'delete-backward-char)
(define-key isearch-mode-map "\C-h" #'isearch-delete-char)


;; overwrite stock bindings
(spacemacs/set-leader-keys
  "<SPC>" #'evil-avy-goto-char-2
  "fY" #'yxl/show-and-copy-buffer-filename-in-projectile
  "ws" #'split-window-below-and-focus
  "wS" #'split-window-below
  "wv" #'split-window-right-and-focus
  "wV" #'split-window-right
  "w <SPC>" #'ace-window)

;; addition to stock bindings
(spacemacs/set-leader-keys
  "hdF" #'describe-face)
