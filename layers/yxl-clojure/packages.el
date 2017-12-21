(setq yxl-clojure-packages
      '(cider
        cider-eval-sexp-fu
        clj-refactor
        clojure-cheatsheet
        clojure-mode
        clojure-snippets
        company
        eldoc
        ggtags
        counsel-gtags
        helm-gtags
        org
        popwin
        sayid
        smartparens
        subword))


(defun yxl-clojure/init-cider ()
  (use-package cider
    :defer t
    :init
    (progn
      (spacemacs/register-repl 'cider 'cider-jack-in "cider")
      (setq cider-stacktrace-default-filters '(tooling dup)
            cider-repl-pop-to-buffer-on-connect nil
            cider-prompt-save-file-on-load nil
            cider-repl-use-clojure-font-lock t
            cider-repl-history-file (concat spacemacs-cache-directory "cider-repl-history"))
      (push "\\*cider-repl\.\+\\*" spacemacs-useful-buffers-regexp)
      (add-hook 'clojure-mode-hook 'cider-mode)
      (dolist (x '(spacemacs-jump-handlers-clojure-mode
                   spacemacs-jump-handlers-clojurec-mode
                   spacemacs-jump-handlers-clojurescript-mode
                   spacemacs-jump-handlers-clojurex-mode
                   spacemacs-jump-handlers-cider-repl-mode))
        (add-to-list x 'spacemacs/clj-find-var))

      (add-hook 'clojure-mode-hook #'spacemacs//init-jump-handlers-clojure-mode)
      (add-hook 'clojurescript-mode-hook #'spacemacs//init-jump-handlers-clojurescript-mode)
      (add-hook 'clojurec-mode-hook #'spacemacs//init-jump-handlers-clojurec-mode)
      (add-hook 'cider-repl-mode-hook #'spacemacs//init-jump-handlers-cider-repl-mode))
    :config
    (progn
      ;; add support for golden-ratio
      (with-eval-after-load 'golden-ratio
        (push 'cider-popup-buffer-quit-function golden-ratio-extra-commands))
      ;; add support for evil
      (evil-set-initial-state 'cider-stacktrace-mode 'motion)
      (evil-set-initial-state 'cider-popup-buffer-mode 'motion)
      (add-hook 'cider--debug-mode-hook 'spacemacs/cider-debug-setup)
      (yxl-clojure/setup-cider-leader-keys)
      (yxl-clojure/setup-cider-evilified-keybindings)

      ;; open cider-doc directly and close it with q
      (setq cider-prompt-for-symbol nil)

      (when clojure-enable-fancify-symbols
        (clojure/fancify-symbols 'cider-repl-mode)
        (clojure/fancify-symbols 'cider-clojure-interaction-mode)))

    (defadvice cider-jump-to-var (before add-evil-jump activate)
      (evil-set-jump))))

(defun yxl-clojure/init-cider-eval-sexp-fu ()
  (with-eval-after-load 'eval-sexp-fu
    (require 'cider-eval-sexp-fu)))

(defun yxl-clojure/init-clj-refactor ()
  (use-package clj-refactor
    :defer t
    :init
    (add-hook 'clojure-mode-hook 'clj-refactor-mode)
    :config
    (progn
      (cljr-add-keybindings-with-prefix "C-c C-f")

      ;; Usually we do not set keybindings in :config, however this must be done
      ;; here because it reads the variable `cljr--all-helpers'. Since
      ;; `clj-refactor-mode' is added to the hook, this should trigger when a
      ;; clojure buffer is opened anyway, so there's no "keybinding delay".
      (let ((clj-refactor--key-binding-prefixes
             '(("mr" . "refactor")
               ("mra" . "add")
               ("mrc" . "cycle/clean/convert")
               ("mrd" . "destructure")
               ("mre" . "extract/expand")
               ("mrf" . "find/function")
               ("mrh" . "hotload")
               ("mri" . "introduce/inline")
               ("mrm" . "move")
               ("mrp" . "project/promote")
               ("mrr" . "remove/rename/replace")
               ("mrs" . "show/sort/stop")
               ("mrt" . "thread")
               ("mru" . "unwind/update"))))
        (dolist (m '(clojure-mode
                     clojurec-mode
                     clojurescript-mode
                     clojurex-mode
                     cider-repl-mode
                     cider-clojure-interaction-mode))
          (mapc (lambda (x) (spacemacs/declare-prefix-for-mode
                              m (car x) (cdr x)))
                clj-refactor--key-binding-prefixes)
          (dolist (r cljr--all-helpers)
            (let* ((binding (car r))
                   (func (cadr r)))
              (when (not (string-prefix-p "hydra" (symbol-name func)))
                (spacemacs/set-leader-keys-for-major-mode m
                  (concat "r" binding) func)))))))))

(defun yxl-clojure/init-clojure-cheatsheet ()
  (use-package clojure-cheatsheet
    :defer t
    :init
    (progn
      (setq sayid--key-binding-prefixes
            '(("mhc" . "clojure-cheatsheet")))
      (dolist (m '(clojure-mode
                   clojurec-mode
                   clojurescript-mode
                   clojurex-mode
                   cider-repl-mode
                   cider-clojure-interaction-mode))
        (mapc (lambda (x) (spacemacs/declare-prefix-for-mode
                            m (car x) (cdr x)))
              sayid--key-binding-prefixes)
        (spacemacs/set-leader-keys-for-major-mode m
          "hc" 'clojure-cheatsheet)))))

(defun yxl-clojure/init-clojure-mode ()
  (use-package clojure-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
      ;; This regexp matches shebang expressions like `#!/usr/bin/env boot'
      (add-to-list 'magic-mode-alist '("#!.*boot\\s-*$" . clojure-mode))
      (dolist (m '(clojure-mode clojurec-mode clojurescript-mode clojurex-mode))
        (spacemacs/set-leader-keys-for-major-mode m
          "fl" 'clojure-align)))
    :config
    (when clojure-enable-fancify-symbols
      (dolist (m '(clojure-mode clojurescript-mode clojurec-mode clojurex-mode))
        (clojure/fancify-symbols m)))))

(defun yxl-clojure/post-init-eldoc ()
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'eldoc-mode)
  (add-hook 'cider-clojure-interaction-mode-hook 'eldoc-mode))

(defun yxl-clojure/pre-init-popwin ()
  (spacemacs|use-package-add-hook popwin
    :post-config
    (push '("*cider-error*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          popwin:special-display-config)
    (push '("*cider-doc*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
          popwin:special-display-config)))

(defun yxl-clojure/post-init-smartparens ()
  (add-hook 'cider-repl-mode-hook
            (if dotspacemacs-smartparens-strict-mode
                #'smartparens-strict-mode
              #'smartparens-mode))
  (with-eval-after-load 'smartparens
    (sp-local-pair 'clojure-mode "`" nil :actions nil)))

(defun yxl-clojure/post-init-subword ()
  (add-hook 'cider-mode-hook 'subword-mode))

;; TODO: Deprecate this after spacemacs major update
(defun yxl-clojure/post-init-company ()
  (spacemacs|add-company-hook cider-mode)
  (spacemacs|add-company-hook cider-repl-mode)
  (push 'company-capf company-backends-cider-mode)
  (push 'company-capf company-backends-cider-repl-mode)
  ;; (spacemacs|add-company-backends
  ;;   :backends company-capf
  ;;   :modes
  ;;   cider-mode
  ;;   cider-repl-mode)
  )

(defun yxl-clojure/post-init-ggtags ()
  (add-hook 'clojure-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun yxl-clojure/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'clojure-mode))

(defun yxl-clojure/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'clojure-mode))

(defun yxl-clojure/init-clojure-snippets ()
  (use-package clojure-snippets
    :defer t))

(defun yxl-clojure/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(clojure . t))
    (setq org-babel-clojure-backend 'cider)))

(defun yxl-clojure/init-sayid ()
  (use-package sayid
    :defer t
    :config
    (progn
      (yxl-clojure/setup-sayid-leader-keys)
      (yxl-clojure/setup-sayid-evilified-keybindings))))
