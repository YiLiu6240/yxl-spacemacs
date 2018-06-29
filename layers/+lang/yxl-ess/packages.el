(setq yxl-ess-packages '(ess
                         (ess-R-object-popup :location local)
                         (yxl-ess :location site)
                         (ess-goodies :location site)
                         (lsp-r :location local
                                :toggle yxl-ess-enable-lsp)
                         ess-view
                         markdown-mode
                         polymode))


(defun yxl-ess/pre-init-ess ()
  (setq-default ess-roxy-re "#+'"))

(defun yxl-ess/init-ess ()
  (use-package ess-site
    :mode (("\\.sp\\'"           . S-mode)
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
           ("\\.[Jj][Mm][Dd]\\'" . ess-jags-mode))
    :commands (R stata SAS julia)
    :init
    (progn
      (spacemacs/register-repl 'ess-site 'R)
      (spacemacs/register-repl 'ess-site 'SAS)
      (spacemacs/register-repl 'ess-site 'stata)
      (spacemacs/register-repl 'ess-site 'julia)
      (add-hook 'inferior-ess-mode-hook
                'spacemacs//ess-fix-read-only-inferior-ess-mode)
      (when (configuration-layer/package-used-p 'company)
        (add-hook 'ess-mode-hook 'company-mode))
      (setq ess-smart-S-assign-key nil)))
  (with-eval-after-load 'ess-mode
    (yxl-ess//setup-general-configs)
    (yxl-ess//setup-imenu)
    (yxl-ess//setup-lintr)
    (add-hook 'ess-mode-hook #'yxl-ess//ess-hook)
    (yxl-ess//setup-ess-mode-keybindings)
    (yxl-ess//setup-leader-keys))
  (with-eval-after-load 'ess-r-mode
    (add-to-list 'spacemacs-useful-buffers-regexp "\\*R:*")
    (add-hook 'R-mode-hook #'yxl-ess//R-hook))
  (with-eval-after-load 'ess-help
    (yxl-ess//setup-ess-help-keybindings))
  (with-eval-after-load 'ess-rdired
    (yxl-ess//setup-rdired)
    (yxl-ess//setup-ess-rdired-keybindings))
  (with-eval-after-load 'ess-julia-mode
    (add-to-list 'spacemacs-useful-buffers-regexp "\\*julia:*")
    (yxl-ess//setup-julia-keybindings)))

(defun yxl-ess/init-ess-R-object-popup ()
  (use-package ess-R-object-popup
    :after ess-site))

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

(defun yxl-ess/init-lsp-r ()
  (use-package lsp-r
    :if yxl-ess-enable-lsp
    :init (add-hook 'R-mode-hook 'lsp-R-enable)
    :after ess-site))

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
