(setq yxl-ess-packages '(ess
                         (ess-R-object-popup :location local)
                         (yxl-ess :location site)
                         (ess-goodies :location site)
                         ess-view
                         markdown-mode
                         polymode))


(defun yxl-ess/pre-init-ess ()
  (setq-default ess-roxy-re "#+'"))

(defun yxl-ess/init-ess ()
  (use-package ess-site
    :mode
    (("\\.sp\\'"           . S-mode)
     ("/R/.*\\.q\\'"       . R-mode)
     ("\\.[qsS]\\'"        . S-mode)
     ("\\.ssc\\'"          . S-mode)
     ("\\.SSC\\'"          . S-mode)
     ("\\.[rR]\\'"         . R-mode)
     ("\\.[rR]nw\\'"       . Rnw-mode)
     ("\\.[sS]nw\\'"       . Snw-mode)
     ("\\.[rR]profile\\'"  . R-mode)
     ("NAMESPACE\\'"       . R-mode)
     ("CITATION\\'"        . R-mode)
     ("\\.omg\\'"          . omegahat-mode)
     ("\\.hat\\'"          . omegahat-mode)
     ("\\.lsp\\'"          . XLS-mode)
     ("\\.do\\'"           . STA-mode)
     ("\\.ado\\'"          . STA-mode)
     ("\\.[Ss][Aa][Ss]\\'" . SAS-mode)
     ("\\.jl\\'"           . ess-julia-mode)
     ("\\.[Ss]t\\'"        . S-transcript-mode)
     ("\\.Sout"            . S-transcript-mode)
     ("\\.[Rr]out"         . R-transcript-mode)
     ("\\.Rd\\'"           . Rd-mode)
     ("\\.[Bb][Uu][Gg]\\'" . ess-bugs-mode)
     ("\\.[Bb][Oo][Gg]\\'" . ess-bugs-mode)
     ("\\.[Bb][Mm][Dd]\\'" . ess-bugs-mode)
     ("\\.[Jj][Aa][Gg]\\'" . ess-jags-mode)
     ("\\.[Jj][Oo][Gg]\\'" . ess-jags-mode)
     ("\\.[Jj][Mm][Dd]\\'" . ess-jags-mode)))
  :init
  (progn
    (spacemacs/register-repl 'ess-site 'julia)
    (spacemacs/register-repl 'ess-site 'R)
    ;; Explicitly run prog-mode hooks since ess-mode does not derive from
    ;; prog-mode major-mode
    (add-hook 'ess-mode-hook 'spacemacs/run-prog-mode-hooks)
    (add-hook 'inferior-ess-mode-hook
              'spacemacs//ess-fix-read-only-inferior-ess-mode)
    (when (configuration-layer/package-used-p 'company)
      (add-hook 'ess-mode-hook 'company-mode)))
  :config
  (progn
    (with-eval-after-load 'ess-site
      (spacemacs/register-repl 'ess-site 'SAS)
      (spacemacs/register-repl 'ess-site 'stata)
      (yxl-ess/setup-general-configs)
      (yxl-ess/setup-imenu)
      (yxl-ess/setup-rdired)
      (yxl-ess/setup-lintr)
      (add-hook 'ess-mode-hook #'yxl-ess/ess-hook)
      (add-hook 'R-mode-hook #'yxl-ess/R-hook)
      (add-hook 'ess-mode-hook 'smartparens-mode)
      (add-hook 'ess-mode-hook 'hl-todo-mode)
      (yxl-ess/setup-general-keybindings)
      (yxl-ess/setup-ess-rdired-leader-keys)
      (yxl-ess/setup-ess-help-leader-keys)
      (yxl-ess/setup-leader-keys)
      (yxl-ess/setup-julia-keybindings))))

(defun yxl-ess/init-ess-R-object-popup ()
  (use-package ess-R-object-popup
    :after (ess-site)))

(defun yxl-ess/init-yxl-ess ()
  (use-package yxl-ess
    :after ess-site))

(defun yxl-ess/init-ess-goodies ()
  (use-package ess-goodies
    :after ess-site))

(defun yxl-ess/init-ess-view ()
  (use-package ess-view
    :after ess-site))

(defun yxl-ess/post-init-markdown-mode ()
  (with-eval-after-load 'markdown-mode
    (progn
      (define-key markdown-mode-map (kbd "M--")
        (lambda () (interactive)
          (yxl-insert-symbol "<-")))
      (define-key markdown-mode-map (kbd "C-S-M")
        (lambda () (interactive)
          (yxl-insert-symbol "%>%"))))))

(defun yxl-ess/post-init-org ()
  (with-eval-after-load 'org
    (progn)))

(defun yxl-ess/init-polymode ()
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
