(setq yxl-edit-packages '(parinfer
                          lispy
                          smartparens
                          imenu-anywhere
                          hl-todo
                          narrow-indirect))

(defun yxl-edit/init-parinfer ()
  (use-package parinfer
    :defer t
    :diminish parinfer-mode
    :init
    (progn
      (add-hook 'emacs-lisp-mode-hook 'parinfer-mode)
      (add-hook 'clojure-mode-hook 'parinfer-mode)
      (add-hook 'common-lisp-mode-hook 'parinfer-mode)
      (add-hook 'scheme-mode-hook 'parinfer-mode)
      (add-hook 'lisp-mode-hook 'parinfer-mode)
      (spacemacs|add-toggle parinfer-indent
        :evil-leader "tP"
        :documentation "Enable Parinfer Indent Mode."
        :if (bound-and-true-p parinfer-mode)
        :status (eq parinfer--mode 'indent)
        :on (parinfer-toggle-mode)
        :off (parinfer-toggle-mode))
      (setq parinfer-extensions '(defaults pretty-parens evil smart-tab smart-yank)))))

(defun yxl-edit/init-lispy ()
  (use-package lispy
    :defer t
    :init
    (progn
      (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1))))
    :config
    (progn
      (define-key emacs-lisp-mode-map
        (kbd "C-<tab>") 'lispy-indent-adjust-parens)
      (define-key emacs-lisp-mode-map
        (kbd "C-S-<tab>") 'lispy-dedent-adjust-parens)
      (define-key lispy-mode-map (kbd "C-2") 'lispy-describe-inline)
      (define-key lispy-mode-map (kbd "C-3") 'lispy-arglist-inline))))
      ;; (define-key lispy-mode-map (kbd "C-k") 'lispy-splice)
      ;; (define-key lispy-mode-map (kbd "C-3") 'lispy-mark-symbol)

(defun yxl-edit/pre-init-smartparens ()
  (spacemacs|use-package-add-hook smartparens
    :post-config
    (progn
      ;; https://www.reddit.com/r/emacs/comments/54agp2/from_an_evil_perspective_how_to_efficiently_edit/
      ;; (define-key sp-keymap (kbd "C-<tab>") 'sp-indent-adjust-sexp)
      ;; (define-key sp-keymap (kbd "C-S-<iso-lefttab>") 'sp-dedent-adjust-sexp)
      (setq sp-highlight-pair-overlay nil)
      (setq sp-highlight-wrap-overlay nil)
      (setq sp-highlight-wrap-tag-overlay nil))))

(defun yxl-edit/post-init-hl-todo ()
  (with-eval-after-load 'hl-todo
    (setq-default hl-todo-keyword-faces
                  `(("HOLD" . (:weight bold :foreground ,(face-attribute 'font-lock-function-name-face :foreground)))
                    ("TODO" . (:weight bold :foreground ,(face-attribute 'font-lock-variable-name-face :foreground)))
                    ("NEXT" . (:weight bold :foreground ,(face-attribute 'font-lock-constant-face :foreground)))
                    ("PROG" . (:weight bold :foreground ,(face-attribute 'font-lock-function-name-face :foreground)))
                    ("WIP" . (:weight bold :foreground ,(face-attribute 'font-lock-function-name-face :foreground)))
                    ("OKAY" . (:weight bold :foreground ,(face-attribute 'success :foreground)))
                    ("DONT" . (:weight bold :foreground ,(face-attribute 'font-lock-builtin-face :foreground)))
                    ("IDEA" . (:weight bold :foreground ,(face-attribute 'font-lock-builtin-face :foreground)))
                    ("DOC" . (:weight bold :foreground ,(face-attribute 'font-lock-builtin-face :foreground)))
                    ("ISSUE" . (:weight bold :foreground ,(face-attribute 'font-lock-warning-face :foreground)))
                    ("FAIL" . (:weight bold :foreground ,(face-attribute 'font-lock-warning-face :foreground)))
                    ("DONE" . (:weight bold :foreground ,(face-attribute 'font-lock-comment-face :foreground)))
                    ("NOTE" . (:weight bold :foreground ,(face-attribute 'font-lock-string-face :foreground)))
                    ("REVIEW" . (:weight bold :foreground ,(face-attribute 'font-lock-string-face :foreground)))
                    ("HACK" . (:weight bold :foreground ,(face-attribute 'font-lock-function-name-face :foreground)))
                    ("PATCH" . (:weight bold :foreground ,(face-attribute 'font-lock-function-name-face :foreground)))
                    ("FIXME" . (:weight bold :foreground ,(face-attribute 'font-lock-variable-name-face :foreground)))
                    ("XXX" . (:weight bold :foreground ,(face-attribute 'font-lock-variable-name-face :foreground)))
                    ("???" . (:weight bold :foreground ,(face-attribute 'font-lock-warning-face :foreground)))))))

(defun yxl-edit/init-imenu-anywhere ()
  (use-package imenu-anywhere
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys "sJ" #'imenu-anywhere))
    :config
    (progn
      ;; remove same-project-p, too confusing
      (setq imenu-anywhere-buffer-filter-functions '(imenu-anywhere-same-mode-p
                                                     imenu-anywhere-friendly-mode-p)))))
(defun yxl-edit/init-narrow-indirect ()
  (use-package narrow-indirect
    :defer t))
