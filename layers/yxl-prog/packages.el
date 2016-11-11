(setq yxl-prog-packages '((prog-mode :location built-in)
                          python
                          cc-mode
                          ;; org
                          ;; ob-ipython
                          ;; company
                          graphviz-dot-mode
                          imenu-anywhere
                          smartparens
                          narrow-indirect
                          bash-completion))
(defun yxl-prog/init-prog-mode ()
  (use-package prog-mode
    :defer t
    :init
    (progn
      (add-hook 'prog-mode-hook 'fci-mode)
      (add-hook 'prog-mode-hook 'hl-todo-mode)
      (add-hook 'prog-mode-hook (lambda ()
                                  (setq indent-tabs-mode nil)
                                  (setq tab-width 4))))))

(defun yxl-prog/post-init-python ()
  (with-eval-after-load 'python
    (add-hook 'python-mode-hook 'which-function-mode)
    ;; (add-hook 'python-mode-hook 'evil-visual-mark-mode)
    ;; (add-hook 'python-mode-hook (lambda () (modify-syntax-entry ?_ "w")))
    )

  ;; python imenu hack ----
  ;; http://stackoverflow.com/questions/21644876/imenu-does-not-work-for-python-mode-or-c-mode
  (with-eval-after-load 'python
    (defvar yxl-python-imenu-expression
      '(("Class" "^class \\(.+\\):$" 1)
        ("Function" "^def \\(.+\\)\(" 1)
        ("Outline" "^\\(## .+\\)$" 1)
        ("Outline" "^\\(### .+\\)$" 1)))

    (defun yxl-python-imenu ()
      "set python imenu items to customized item list"
      (interactive)
      (imenu--generic-function yxl-python-imenu-expression))

    (add-hook
     'python-mode-hook
     (lambda ()
       (setq imenu-create-index-function 'yxl-python-imenu)))))

(defun yxl-prog/post-init-cc-mode ()
  (with-eval-after-load 'cc-mode
    (add-hook 'c++-mode-hook 'which-function-mode)))

(defun yxl-prog/post-init-graphviz-dot-mode ()
  ;; copy from zilongshanren
  (with-eval-after-load 'graphviz-dot-mode
    (require 'company-keywords)
    (push '(graphviz-dot-mode  "digraph" "node" "shape" "subgraph"
                               "label" "edge" "bgcolor" "style"
                               "record") company-keywords-alist)))

;; ;; literate programming
;; ;; from builtin ess layer
;; (defun yxl-prog/post-init-org ()
;;   (spacemacs|use-package-add-hook org
;;     :post-config
;;     (progn
;;       (add-to-list 'org-babel-load-languages '(dot . t))
;;       (add-to-list 'org-babel-load-languages '(latex . t))
;;       (add-to-list 'org-babel-load-languages '(octave . t))
;;       (add-to-list 'org-babel-load-languages '(org . t))
;;       (add-to-list 'org-babel-load-languages '(perl . t))
;;       (add-to-list 'org-babel-load-languages '(python . t))
;;       (add-to-list 'org-babel-load-languages '(ruby . t))
;;       (add-to-list 'org-babel-load-languages '(sh . t))
;;       (add-to-list 'org-babel-load-languages '(sqlite . t))
;;       (setq org-confirm-babel-evaluate nil)
;;       (setq org-src-preserve-indentation t)
;;       (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
;;       (add-hook 'org-mode-hook 'org-display-inline-images))))

;; (defun yxl-prog/init-ob-ipython ()
;;   (use-package ob-ipython
;;     :defer t
;;     :config
;;     (org-babel-do-load-languages
;;      'org-babel-load-languages
;;      '((ipython . t)))))

(defun yxl-prog/init-imenu-anywhere ()
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

(defun yxl-prog/pre-init-smartparens ()
  (spacemacs|use-package-add-hook smartparens
    :post-config
    (progn
      ;; https://www.reddit.com/r/emacs/comments/54agp2/from_an_evil_perspective_how_to_efficiently_edit/
      (define-key sp-keymap (kbd "C-<tab>") 'sp-indent-adjust-sexp)
      (define-key sp-keymap (kbd "C-S-<tab>") 'sp-dedent-adjust-sexp)
      (setq sp-highlight-pair-overlay nil)
      (setq sp-highlight-wrap-overlay nil)
      (setq sp-highlight-wrap-tag-overlay nil))))

;; (defun yxl-prog/init-lispy ()
;;   (use-package lispy
;;     :defer t
;;     :init
;;     (progn
;;       (add-hook 'emacs-lisp-mode-hook #'lispy-mode)
;;       (spacemacs|add-toggle lispy
;;         :status lispy-mode
;;         :on (lispy-mode 1)
;;         :off (lispy-mode -1)
;;         :documentation "toggle lispy"
;;         :evil-leader "tol"))
;;     :config
;;     (progn
;;       (define-key emacs-lisp-mode-map (kbd "C-<tab>") 'lispy-indent-adjust-parens)
;;       (define-key emacs-lisp-mode-map (kbd "C-S-<tab>") 'lispy-dedent-adjust-parens)
;;       (define-key lispy-mode-map (kbd "C-2") 'lispy-describe-inline)
;;       (define-key lispy-mode-map (kbd "C-3") 'lispy-arglist-inline)
;;       ;; (define-key lispy-mode-map (kbd "C-k") 'lispy-splice)
;;       ;; (define-key lispy-mode-map (kbd "C-3") 'lispy-mark-symbol)
;;       )))

(defun yxl-prog/init-narrow-indirect ()
  (use-package narrow-indirect
    :defer t))

(defun yxl-prog/init-bash-completion ()
  (use-package bash-completion
    :defer t
    :init
    (progn
      (bash-completion-setup))))
