(global-set-key (kbd "s-3") (lambda () (interactive) (insert "#")))
(global-set-key (kbd "C-S-p") #'helm-M-x)
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
  "wV" #'split-window-right
  ;; TODO: change these after new version
  "jb" #'avy-pop-mark
  "jj" #'evil-avy-goto-char-2
  "jJ" #'evil-avy-goto-char
  "jl" #'evil-avy-goto-line
  "ju" #'spacemacs/avy-goto-url
  "jw" #'evil-avy-goto-word-or-subword-1
  "js" #'sp-split-sexp
  "jn" #'sp-newline
  "xo" #'spacemacs/avy-open-url)
