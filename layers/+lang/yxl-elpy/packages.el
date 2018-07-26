;; -*- lexical-binding: t -*-
(setq yxl-elpy-packages '(python
                          elpy
                          jedi))

(defun yxl-elpy/init-python ()
  (use-package python
    :defer t
    :mode (("\\.py\\'" . python-mode)
           ("\\.ipy\\'" . python-mode))
    :init
    (setq-default python-indent-offset 4)
    :config
    (add-hook 'inferior-python-mode-hook
              #'yxl-elpy//inferior-python-mode-hook-setup)))

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
