(setq yxl-ui-packages '(airline-themes
                        neotree
                        treemacs
                        (yxl-airline :location site
                                     :toggle (eq yxl-ui-modeline 'yxl-airline))
                        (doom-modeline :toggle (eq yxl-ui-modeline 'doom-modeline))))

(defun yxl-ui/init-airline-themes ()
  (use-package airline-themes
    :ensure t
    :config
    (progn
      (setq airline-cursor-colors t)
      (setq airline-display-directory nil))))

(defun yxl-ui/init-yxl-airline ()
  (use-package yxl-airline
    :if (eq yxl-ui-modeline 'yxl-airline)
    :hook (spacemacs-post-user-config . (lambda () (load-theme 'yxl-airline t)))
    :config
    (progn
      (add-hook 'spacemacs-post-theme-change-hook
                #'yxl-airline-theme-set-colors))))

(defun yxl-ui/init-doom-modeline ()
  (use-package doom-modeline
    :defer t
    :if (eq yxl-ui-modeline 'doom-modeline)
    :hook (after-init . doom-modeline-init)
    :config
    (progn
      (setq doom-modeline-height 25))))

(defun yxl-ui/post-init-neotree ()
  (defun yxl-neotree-enter-external ()
    "Open with a program from a list of registered programs."
    (interactive)
    (neo-buffer--execute nil 'yxl-neo-open-file-external 'neo-open-dir))
  (defun yxl-neo-open-file-external (full-path arg)
    "Open with a program from a list of registered programs."
    (yxl-open-file-external full-path))
  (with-eval-after-load 'neotree
    (define-key neotree-mode-map "o" #'spacemacs/neotree-expand-or-open)
    (define-key neotree-mode-map "O" #'neotree-enter-ace-window)
    (define-key neotree-mode-map "x" #'yxl-neotree-enter-external)))

(defun yxl-ui/post-init-treemacs ()
  (setq treemacs-no-png-images t)
  (setq treemacs-width 20)
  (setq treemacs-indentation 0)
  (with-eval-after-load 'treemacs
    (define-key treemacs-mode-map "x" #'yxl-treemacs-visit-node-external)
    (define-key treemacs-mode-map "i" #'treemacs-TAB-action)))
