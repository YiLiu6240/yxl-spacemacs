(setq yxl-prog-packages '((prog-mode :location built-in)
                          python
                          cc-mode
                          org
                          ob-ipython
                          company
                          graphviz-dot-mode
                          imenu-anywhere))

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
    (defun python-start-or-switch-repl ()
      "Start and/or switch to the REPL."
      (interactive)
      (let ((shell-process
             (or (python-shell-get-process)
                 ;; `run-python' has different return values and different
                 ;; errors in different emacs versions. In 24.4, it throws an
                 ;; error when the process didn't start, but in 25.1 it
                 ;; doesn't throw an error, so we demote errors here and
                 ;; check the process later
                 (with-demoted-errors "Error: %S"
                   ;; in Emacs 24.5 and 24.4, `run-python' doesn't return the
                   ;; shell process
                   (call-interactively #'run-python)
                   (python-shell-get-process)))))
        (unless shell-process
          (error "Failed to start python shell properly"))
        (pop-to-buffer (process-buffer shell-process))
        (evil-insert-state)))
    ;; (add-hook 'python-mode-hook 'evil-visual-mark-mode)
    ;; (add-hook 'python-mode-hook (lambda () (modify-syntax-entry ?_ "w")))
    )

  ;; python imenu hack ----
  ;; http://stackoverflow.com/questions/21644876/imenu-does-not-work-for-python-mode-or-c-mode
  (with-eval-after-load 'python
    (defvar yxl/python-imenu-expression
      '(("Class" "^class \\(.+\\):$" 1)
        ("Function" "^def \\(.+\\)\(" 1)
        ("Outline" "^\\(## .+\\)$" 1)
        ("Outline" "^\\(### .+\\)$" 1)))

    (defun yxl/python-imenu ()
      "set python imenu items to customized item list"
      (interactive)
      (imenu--generic-function yxl/python-imenu-expression))

    (add-hook
     'python-mode-hook
     (lambda ()
       (setq imenu-create-index-function 'yxl/python-imenu)))))

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

;; literate programming
;; from builtin ess layer
(defun yxl-prog/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config
    (progn
      (add-to-list 'org-babel-load-languages '((dot . t)
                                               (latex . t)
                                               (octave . t)
                                               (org . t)
                                               (perl . t)
                                               (python . t)
                                               (ruby . t)
                                               (sh . t)
                                               (sqlite . t))))
    (setq org-confirm-babel-evaluate nil)
    (setq org-src-preserve-indentation t)
    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
    (add-hook 'org-mode-hook 'org-display-inline-images)))

(defun yxl-prog/init-ob-ipython ()
  (use-package ob-ipython
    :defer t
    :config
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((ipython . t)))))

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
