(setq yxl-datascience-packages '((counsel-dash)
                                 (yxl-dash :location site)
                                 (yxl-doc-portal :location site)
                                 ess
                                 (yxl-ess :location site)
                                 (ess-goodies :location site)
                                 ess-view
                                 markdown-mode
                                 org-mode
                                 polymode
                                 python
                                 ein))

(defun yxl-datascience/init-counsel-dash ()
  (use-package counsel-dash
    :commands (helm-dash-installed-docsets)))

(defun yxl-datascience/init-yxl-dash ()
  (use-package yxl-dash
    :init
    (progn
      (spacemacs/set-leader-keys
        "dH" #'yxl-dash-search-docset
        "dh" #'yxl-dash-search-docset-external-browser
        "dm" #'yxl-dash-search-docset-chromium
        "df" #'yxl-dash-search-docset-firefox
        "dr" #'counsel-dash-reset-connections))
    :commands (yxl-dash-search-docset
               yxl-dash-search-docset-external-browser
               yxl-dash-search-docset-chromium
               yxl-dash-search-docset-firefox
               yxl-dash-search-docset-helm)
    :config
    (progn
      (require 'counsel-dash)
      (setq yxl-dash-docset-path "~/Dropbox/dash-docsets")
      (setq yxl-dash-browser-func 'w3m-goto-url-new-session)
      (setq counsel-dash-browser-func yxl-dash-browser-func)
      (yxl-dash-activate-package-docsets yxl-dash-docset-path))))

(defun yxl-datascience/init-yxl-doc-portal ()
  (use-package yxl-doc-portal
    :defer t
    :commands (yxl-doc-portal)
    :init
    (progn
      (spacemacs/set-leader-keys
        "dp" #'yxl-doc-portal
        "dP" #'yxl-doc-portal-chromium))
    :config
    (progn
      (setq yxl-dp-docs
            (delete-dups (sort (append yxl-dp-docs
                                       yxl-datascience-additional-docs)
                               (lambda (elem1 elem2)
                                 (let ((str1 (car elem1))
                                       (str2 (car elem2)))
                                   (string-lessp str1 str2)))))))))

(defun yxl-datascience/pre-init-ess ()
  (setq-default ess-roxy-re "#+'"))

(defun yxl-datascience/post-init-ess ()
  (with-eval-after-load 'ess-site
    (yxl-datascience/ess-setup-keybindings)
    (yxl-datascience/ess-setup-imenu)
    (yxl-datascience/ess-setup-rdired)
    (yxl-datascience/setup-ess-rdired-hydra)
    (yxl-datascience/ess-setup-help)
    (yxl-datascience/ess-setup-lintr)
    (add-hook 'ess-mode-hook #'yxl-datascience/ess-hook)
    (add-hook 'R-mode-hook #'yxl-datascience/R-hook)
    (add-hook 'ess-mode-hook 'smartparens-mode)
    ;; (add-hook 'ess-mode-hook 'fci-mode)
    (add-hook 'ess-mode-hook 'hl-todo-mode)
    (mapcar #'yxl-datascience/ess-set-leader-keys
            '(ess-mode ess-julia-mode inferior-ess-mode))
    (mapcar #'yxl-datascience/ess-declare-prefix
            '(ess-mode ess-julia-mode inferior-ess-mode))
    (yxl-datascience/setup-julia-keybindings)
    (setq spacemacs-useful-buffers-regexp
          (append spacemacs-useful-buffers-regexp '("\\*R:*" "\\*julia:*")))
    (setq ess-history-file nil)
    ;; no spaces around argument assignment
    (setq ess-R-argument-suffix " = ")
    (setq ess-eval-visibly 'nowait)
    (setq ess-execute-in-process-buffer t)
    (setq ess-ask-for-ess-directory nil)
    (setq yxl-ess-style '((ess-indent-offset . 2)
                          (ess-offset-arguments . open-delim)
                          (ess-offset-arguments-newline . prev-call)
                          (ess-offset-block . prev-line)
                          (ess-offset-continued . straight)
                          (ess-align-nested-calls "ifelse")
                          (ess-align-arguments-in-calls "function[    ]*(")
                          (ess-align-continuations-in-calls . t)
                          (ess-align-blocks control-flow)
                          (ess-indent-from-lhs arguments fun-decl-opening)
                          (ess-indent-from-chain-start . t)
                          (ess-indent-with-fancy-comments)))
    (ess-add-style 'yxl-ess-style yxl-ess-style)
    ;; (setq ess-default-style 'yxl-ess-style)
    (setq ess-default-style 'RStudio)
    (setq ess-fl-keyword:operators (cons "[-=+></%$!(::)]+"
                                         'font-lock-constant-face))
    (setq ess-R-font-lock-keywords
          '((ess-R-fl-keyword:modifiers  . t)
            (ess-R-fl-keyword:fun-defs   . t)
            (ess-R-fl-keyword:keywords   . t)
            (ess-R-fl-keyword:assign-ops . t)
            (ess-R-fl-keyword:constants  . t)
            (ess-fl-keyword:fun-calls . t)
            (ess-fl-keyword:numbers . t)
            (ess-fl-keyword:operators . t)
            (ess-fl-keyword:delimiters . t)
            (ess-fl-keyword:= . t)
            (ess-R-fl-keyword:F&T . t)
            (ess-R-fl-keyword:%op% . t)))))

(defun yxl-datascience/init-yxl-ess ()
  (use-package yxl-ess
    :after ess-site))

(defun yxl-datascience/init-ess-goodies ()
  (use-package ess-goodies
    :after ess-site))

(defun yxl-datascience/init-ess-view ()
  (use-package ess-view
    :after ess-site))

(defun yxl-datascience/post-init-markdown-mode ()
  (with-eval-after-load 'markdown-mode
    (progn
      (define-key markdown-mode-map (kbd "C-S-M")
        (lambda () (interactive)
          (yxl-insert-symbol "%>%"))))))

(defun yxl-datascience/post-init-org-mode ()
  (with-eval-after-load 'org
    (progn
      (define-key org-mode-map (kbd "C-S-M")
        (lambda () (interactive)
          (yxl-insert-symbol "%>%")))
      (define-key org-mode-map (kbd "S-RET")
        (lambda () (interactive)
          (yxl-insert-symbol "%>%"))))))

(defun yxl-datascience/init-polymode ()
  (use-package polymode
    ;; :mode (("\\.Rmd"   . Rmd-mode))
    :init
    (progn
      ;; TODO: try to toggle Rmd-mode
      (defun Rmd-mode ()
        "ESS Markdown mode for Rmd files"
        (interactive)
        (require 'poly-R)
        (require 'poly-markdown)
        (R-mode)
        (poly-markdown+r-mode)))))

(defun yxl-datascience/post-init-python ()
  (with-eval-after-load 'python
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      ";" #'python-shell-send-string)))

(defun yxl-datascience/init-ein ()
  (use-package ein
    :defer t
    :commands ein:notebooklist-open
    :init
    (progn
      (spacemacs/set-leader-keys "ajn" #'ein:notebooklist-open)
      (spacemacs/set-leader-keys "ajN" #'ein:notebooklist-login)
      (spacemacs/declare-prefix "aj" "jupyter-notebook"))
    :config
    (progn
      (require 'ein-cell-edit)
      (require 'ein-notebook)
      (require 'ein-multilang)
      (setq spacemacs-useful-buffers-regexp
            (append spacemacs-useful-buffers-regexp '("\\*ein:*")))
      (setq ein:completion-backend 'ein:use-company-backend)
      (advice-add 'ein:notebook-save-notebook
                  :override #'ein:notebook-save-notebook-override)
      (add-hook 'ein:notebook-multilang-mode-hook
                #'smartparens-mode)
      (advice-add 'ein:edit-cell-exit
                  :override #'ein:edit-cell-exit-override)
      ;; no idea why we have to enforce evilified state on pager to work
      (add-hook 'ein:pager-mode-hook #'evil-evilified-state)
      (yxl-datascience/setup-jupyter-keybindings)
      (yxl-datascience/setup-jupyter-evilified-keybindings)
      (yxl-datascience/setup-jupyter-leader-keys)
      (yxl-datascience/setup-jupyter-hydra))))
