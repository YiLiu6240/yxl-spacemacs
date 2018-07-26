;; -*- lexical-binding: t -*-
;;
;; Packages that should not be set (included in elpy):
;; - company
;; - eldoc
;; - importmagic
;;
;; Packages need testing and assessment
;; - nose
;; - pyenv
;; - pyvenv
;; - pytest

(setq yxl-elpy-packages '((python :location built-in)
                          elpy
                          jedi
                          cython-mode
                          evil-matchit
                          flycheck
                          helm-cscope
                          helm-gtags
                          ggtags
                          helm-pydoc
                          live-py-mode
                          org
                          pipenv
                          pip-requirements
                          pippel
                          py-isort
                          (pylookup :location local)
                          semantic
                          smartparens
                          stickyfunc-enhance
                          xcscope
                          yapfify))

(defun yxl-elpy/init-python ()
  (use-package python
    :defer t
    :mode (("\\.py\\'" . python-mode)
           ("\\.ipy\\'" . python-mode))
    :init
    (progn
      (setq-default python-indent-offset 4)
      (spacemacs/register-repl
       'python
       'spacemacs/python-start-or-switch-repl "python")
      (spacemacs/add-to-hook 'python-mode-hook
                             '(spacemacs//python-default))
      ;; call `spacemacs//python-setup-shell' once, don't put it in a hook
      ;; (see issue #5988)
      (spacemacs//python-setup-shell))
    :config
    (progn
      (add-hook 'inferior-python-mode-hook
                #'yxl-elpy//inferior-python-mode-hook-setup))))

(defun yxl-elpy/init-jedi ()
  (use-package jedi
    :after python))

(defun yxl-elpy/init-elpy ()
  (use-package elpy
    :after (python jedi)
    :config
    (progn
      ;; enable elpy
      (elpy-enable)
      ;; set lighter
      (diminish 'elpy-mode " Ⓔ")
      ;; configure auto-completion
      (setq elpy-rpc-backend "jedi")
      (add-hook  'elpy-mode-hook #'yxl-elpy//elpy-mode-hook-setup)
      (yxl-elpy//setup-python-mode-leader-keys)
      (yxl-elpy//setup-inferior-python-mode-leader-keys)
      (with-eval-after-load 'counsel
        (define-key inferior-python-mode-map (kbd "C-r")
          'counsel-shell-history))
      (with-eval-after-load 'helm
        (define-key inferior-python-mode-map (kbd "C-r")
          'spacemacs/helm-shell-history)))))

(defun yxl-elpy/init-cython-mode ()
  (use-package cython-mode
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'cython-mode
        "hh" 'anaconda-mode-show-doc
        "gu" 'anaconda-mode-find-references))))

(defun yxl-elpy/post-init-evil-matchit ()
  (add-hook 'python-mode-hook 'turn-on-evil-matchit-mode))

(defun yxl-elpy/post-init-flycheck ()
  (with-eval-after-load 'elpy
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))
  (spacemacs/enable-flycheck 'python-mode))

(defun yxl-elpy/pre-init-helm-cscope ()
  (spacemacs|use-package-add-hook xcscope
    :post-init
    (spacemacs/setup-helm-cscope 'python-mode)))

(defun yxl-elpy/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'python-mode))

(defun yxl-elpy/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'python-mode))

(defun yxl-elpy/post-init-ggtags ()
  (add-hook 'python-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun yxl-elpy/init-helm-pydoc ()
  (use-package helm-pydoc
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'python-mode "hd" 'helm-pydoc)))

(defun yxl-elpy/init-live-py-mode ()
  (use-package live-py-mode
    :defer t
    :commands live-py-mode
    :init
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      "l" 'live-py-mode)))

(defun yxl-elpy/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(python . t))))

(defun yxl-elpy/init-pipenv ()
  (use-package pipenv
    :commands (pipenv-activate
               pipenv-deactivate
               pipenv-shell
               pipenv-open
               pipenv-install
               pipenv-uninstall)
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "vpa" 'pipenv-activate
        "vpd" 'pipenv-deactivate
        "vpi" 'pipenv-install
        "vpo" 'pipenv-open
        "vps" 'pipenv-shell
        "vpu" 'pipenv-uninstall))))

(defun yxl-elpy/init-pip-requirements ()
  (use-package pip-requirements
    :defer t))

(defun yxl-elpy/init-pippel ()
  (use-package pippel
    :defer t
    :init (spacemacs/set-leader-keys-for-major-mode 'python-mode
            "P" 'pippel-list-packages)
    :config
    (evilified-state-evilify-map pippel-package-menu-mode-map
      :mode pippel-package-menu-mode)))

(defun yxl-elpy/init-py-isort ()
  (use-package py-isort
    :defer t
    :init
    (progn
      ;; (add-hook 'before-save-hook 'spacemacs//python-sort-imports)
      (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "rI" 'py-isort-buffer))))

(defun yxl-elpy/init-pylookup ()
  (use-package pylookup
    :commands (pylookup-lookup pylookup-update pylookup-update-all)
    :init
    (progn
      (evilified-state-evilify pylookup-mode pylookup-mode-map)
      (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "hH" 'pylookup-lookup))
    :config
    (progn
      (let ((dir (configuration-layer/get-layer-local-dir 'python)))
        (setq pylookup-dir (concat dir "pylookup/")
              pylookup-program (concat pylookup-dir "pylookup.py")
              pylookup-db-file (concat pylookup-dir "pylookup.db")))
      (setq pylookup-completing-read 'completing-read))))

(defun yxl-elpy/post-init-semantic ()
  (spacemacs/add-to-hook
   'python-mode-hook
   '(semantic-mode
     spacemacs//python-imenu-create-index-use-semantic-maybe))
  (defadvice semantic-python-get-system-include-path
      (around semantic-python-skip-error-advice activate)
    "Don't cause error when Semantic cannot retrieve include
paths for Python then prevent the buffer to be switched. This
issue might be fixed in Emacs 25. Until then, we need it here to
fix this issue."
    (condition-case-unless-debug nil
        ad-do-it
      (error nil))))

(defun ylx-elpy/pre-init-smartparens ()
  (spacemacs|use-package-add-hook smartparens
    :post-config
    (defadvice python-indent-dedent-line-backspace
        (around python/sp-backward-delete-char activate)
      (let ((pythonp (or (not smartparens-strict-mode)
                         (char-equal (char-before) ?\s))))
        (if pythonp
            ad-do-it
          (call-interactively 'sp-backward-delete-char))))))

(defun ylx-elpy/post-init-smartparens ()
  (add-hook 'inferior-python-mode-hook 'smartparens-mode))

(defun ylx-elpy/post-init-stickyfunc-enhance ()
  (add-hook 'python-mode-hook 'spacemacs/load-stickyfunc-enhance))

(defun ylx-elpy/pre-init-xcscope ()
  (spacemacs|use-package-add-hook xcscope
    :post-init
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      "gi" 'cscope/run-pycscope)))

(defun yxl-elpy/init-yapfify ()
  (use-package yapfify
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "=" 'yapfify-buffer)
      (when python-enable-yapf-format-on-save
        (add-hook 'python-mode-hook 'yapf-mode)))
    :config (spacemacs|hide-lighter yapf-mode)))
