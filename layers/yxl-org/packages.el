(setq yxl-org-packages '(org
                         (yxl-org :location site)
                         (org-display-inline-images-with-background :location site)
                         org-gcal
                         ivy-todo))

(defun yxl-org/post-init-org ()
  ;; misc settings
  (with-eval-after-load 'org
    (add-hook 'org-mode-hook 'smartparens-mode)
    ;; (add-hook 'org-mode-hook 'org-bullets-mode)
    (add-hook 'org-mode-hook 'yxl-org/org-mode-hook))
  ;; inject my own configs
  (with-eval-after-load 'org
    (yxl-org/setup-general)
    (yxl-org/setup-bindings)
    (yxl-org/setup-capture)
    (yxl-org/setup-keywords)
    (add-hook 'org-font-lock-set-keywords-hook
              #'yxl-org/setup-extra-fontlock)
    (yxl-org/setup-latex)
    (yxl-org/setup-babel))
  (with-eval-after-load 'org-agenda
    (yxl-org/setup-agenda)))

(defun yxl-org/init-yxl-org ()
  (use-package yxl-org
    :after (org)
    :commands (yxl-org-open-all-task-files)
    :config
    (progn
      (setq yxl-org-task-files yxl-env-org-files))))

(defun yxl-org/init-org-display-inline-images-with-background ()
  (use-package org-display-inline-images-with-background
    :after (org)
    :config
    (progn
      (advice-add 'org-display-inline-images :override
                  #'org-display-inline-images-with-background))))

(defun yxl-org/init-org-gcal ()
  (use-package org-gcal
    :defer t
    :commands (org-gcal-sync)
    :config
    (progn
      (setq package-check-signature nil)
      ;; replace packge builtin `org-gcal--notify'
      (defun my-org-gcal-notify (title mes)
        (message "org-gcal::%s - %s" title mes))
      (fset 'org-gcal--notify 'my-org-gcal-notify))))

(defun yxl-org/init-ivy-todo ()
  (use-package ivy-todo
    :defer t
    :config
    (progn
      (setq ivy-todo-file (expand-file-name "quick.org" yxl-path-org))
      (setq ivy-todo-guess-list nil)
      (setq ivy-todo-headline '("quick-todo"
                                "inbox"))
      (defun ivy-todo-visit (headline)
        (let ((headline-pos (cdr ivy-todo-headline)))
          (find-file ivy-todo-file)
          (ivy-todo--old-or-new-item headline headline-pos)))
      (ivy-add-actions
       'ivy-todo
       '(("v" ivy-todo-visit "visit"))))))
