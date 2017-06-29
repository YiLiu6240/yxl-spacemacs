(setq yxl-prog-packages '((prog-mode :location built-in)
                          python
                          cc-mode
                          ob-ipython
                          graphviz-dot-mode
                          bash-completion
                          scala-mode))

;; TODO: wrong spec, dont use use-package
(defun yxl-prog/init-prog-mode ()
  (use-package prog-mode
    :defer t
    :init
    (progn
      (add-hook 'prog-mode-hook 'fci-mode)
      (add-hook 'prog-mode-hook 'hl-todo-mode)
      (setq-default lua-indent-level 4)
      (setq-default c-basic-offset 4)

      (add-hook 'makefile-mode-hook
                (lambda ()
                  (setq-local indent-tabs-mode t)
                  (setq-local tab-width 4))))))

(defun yxl-prog/post-init-python ()
  (with-eval-after-load 'python))
  ;; (add-hook 'python-mode-hook 'evil-visual-mark-mode)
  ;; (add-hook 'python-mode-hook (lambda () (modify-syntax-entry ?_ "w")))

  ;; python imenu hack ----
  ;; http://stackoverflow.com/questions/21644876/imenu-does-not-work-for-python-mode-or-c-mode
  ;; (with-eval-after-load 'python
  ;;   (defvar yxl-python-imenu-expression
  ;;     '(("Class" "^class \\(.+\\):$" 1)
  ;;       ("Function" "^def \\(.+\\)\(" 1)
  ;;       ("Outline" "^\\(## .+\\)$" 1)
  ;;       ("Outline" "^\\(### .+\\)$" 1)))

  ;;   (defun yxl-python-imenu ()
  ;;     "set python imenu items to customized item list"
  ;;     (interactive)
  ;;     (imenu--generic-function yxl-python-imenu-expression))

    ;; (add-hook
    ;;  'python-mode-hook
    ;;  (lambda ()
    ;;    (setq imenu-create-index-function 'yxl-python-imenu)))))

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
  (use-package ob-ipython))
;; TODO: defer this
;; :defer t
;; :commands (org-babel-ipython-initiate-session
;;            org-babel-load-session:ipython
;;            org-babel-prep-session:ipython
;;            org-babel-execute:ipython)))

(defun yxl-prog/init-bash-completion ()
  (use-package bash-completion
    :defer t
    :init
    (progn
      (bash-completion-setup))))

(defun yxl-prog/init-ob-scala ()
  (use-package ob-scala))

(defun yxl-prog/init-scala-mode ()
  (with-eval-after-load 'scala-mode
    (setq ensime-startup-notification nil)
    (setq ensime-startup-snapshot-notification nil)))
