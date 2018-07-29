(setq yxl-prog-packages '((prog-mode :location built-in)
                          org
                          python
                          ess
                          cc-mode
                          (ob-ipython :location local)
                          (ob-ipython-goodies :location local)
                          graphviz-dot-mode
                          bash-completion
                          scala-mode
                          suggest
                          (sh-script :location built-in)))

(defun yxl-prog/init-prog-mode ()
  (use-package prog-mode
    :config
    (progn
      ;; (add-hook 'prog-mode-hook 'fci-mode)
      (add-hook 'prog-mode-hook 'hl-todo-mode)
      (add-hook 'prog-mode-hook #'spacemacs/toggle-line-numbers-on)
      (setq-default standard-indent 2)
      (setq-default lua-indent-level 2)
      (setq-default c-basic-offset 2)
      (setq-default css-indent-offset 2)

      (add-hook 'makefile-mode-hook
                (lambda ()
                  (setq-local indent-tabs-mode t)
                  (setq-local tab-width 4))))))

(defun yxl-prog/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config
    (progn
      (add-to-list 'org-babel-load-languages '(scala . t))
      (add-to-list 'org-babel-load-languages '(R . t))
      (add-to-list 'org-babel-load-languages '(ipython . t))
      (add-to-list 'org-babel-load-languages '(python . t)))))

;; (defun yxl-prog/pre-init-python ()
;;   (add-to-list 'exec-path (expand-file-name "~/anaconda3/bin")))

(defun yxl-prog/post-init-python ()
  (with-eval-after-load 'python
    ;; See
    ;; https://github.com/jorgenschaefer/elpy/issues/887
    (setq python-shell-completion-native-enable nil)
    (define-key python-mode-map (kbd "C-c f")
      #'yxl-prog/evil-wrap-line-f-print)
    (define-key python-mode-map (kbd "C-c F")
      #'yxl-prog/evil-wrap-line-f)))

(defun yxl-prog/post-init-ess ()
  (with-eval-after-load 'ess-site
    (define-key ess-mode-map (kbd "C-c f")
      #'yxl-prog/evil-wrap-line-f)
    (define-key ess-mode-map (kbd "C-c F")
      #'yxl-prog/evil-wrap-line-f-print)))

(defun yxl-prog/post-init-cc-mode ()
  (with-eval-after-load 'cc-mode))

(defun yxl-prog/post-init-graphviz-dot-mode ()
  ;; copy from zilongshanren
  (with-eval-after-load 'graphviz-dot-mode
    (require 'company-keywords)
    (push '(graphviz-dot-mode  "digraph" "node" "shape" "subgraph"
                               "label" "edge" "bgcolor" "style"
                               "record") company-keywords-alist)))

(defun yxl-prog/init-ob-ipython ()
  (use-package ob-ipython
    :after (org)
    :config
    (progn
      (mapcar
       (lambda (mode)
         (spacemacs/declare-prefix-for-mode mode
           "mo" "ob-ipython"))
       '(python-mode inferior-python-mode))
      (mapcar
       (lambda (mode)
         (spacemacs/set-leader-keys-for-major-mode mode
           "oh" #'ob-ipython-inspect
           "ok" #'ob-ipython-signature-function))
       '(python-mode inferior-python-mode)))))

(defun yxl-prog/init-ob-ipython-goodies ()
  (use-package ob-ipython-goodies
    :after (ob-ipython)))

(defun yxl-prog/init-bash-completion ()
  (use-package bash-completion
    :defer t
    :init
    (progn
      (bash-completion-setup))))

(defun yxl-prog/init-ob-scala ()
  (use-package ob-scala
    :after (org)))

(defun yxl-prog/post-init-scala-mode ()
  (with-eval-after-load 'scala-mode
    (setq ensime-startup-notification nil)
    (setq ensime-startup-snapshot-notification nil)
    (define-key scala-mode-map
      (kbd "M--") (lambda nil (interactive)
                    (if (equal (string (preceding-char)) " ")
                        (insert "<- ")
                      (insert " <- "))))
    (define-key scala-mode-map
      (kbd "M-=") (lambda nil (interactive)
                    (if (equal (string (preceding-char)) " ")
                        (insert "=> ")
                      (insert " => "))))))

(defun yxl-prog/init-suggest ()
  (use-package suggest
    :defer t))

(defun yxl-prog/post-init-sh-script ()
  (with-eval-after-load 'sh-script
    (spacemacs/set-leader-keys-for-major-mode 'sh-mode
      "ss" #'sh-send-line-or-region
      "sS" #'sh-send-line-or-region-and-step)))
