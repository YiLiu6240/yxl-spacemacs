(global-set-key (kbd "s-3") (lambda () (interactive) (insert "#")))
;; (global-set-key (kbd "C-S-p") #'helm-M-x)
(global-set-key (kbd "C-S-p") #'counsel-M-x)
(global-set-key (kbd "C-h") #'delete-backward-char)
; REVIEW: check if C-h still works in ex mode
;; (define-key minibuffer-local-map (kbd "C-h") #'backward-delete-char)
(define-key isearch-mode-map "\C-h" #'isearch-delete-char)


(spacemacs/set-leader-keys
  ;; spacemacs stock
  "<SPC>" #'evil-avy-goto-char-2
  "ws" #'split-window-below-and-focus
  "wS" #'split-window-below
  "wv" #'split-window-right-and-focus
  "wV" #'split-window-right)
