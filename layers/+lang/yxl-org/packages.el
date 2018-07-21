(setq yxl-org-packages '(org
                         (yxl-org :location site)
                         (org-goodies :location local)
                         (org-babel-goodies :location local)
                         (org-display-inline-images-with-background :location site)
                         org-gcal
                         ivy-todo
                         (org-recipes :location site)))

(defun yxl-org/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config
    (when yxl-org-babel-languages
      (mapc (lambda (lang)
              (add-to-list 'org-babel-load-languages lang))
            yxl-org-babel-languages))))

(defun yxl-org/post-init-org ()
  ;; misc settings
  (with-eval-after-load 'org
    (add-hook 'org-mode-hook 'smartparens-mode)
    ;; (add-hook 'org-mode-hook 'org-bullets-mode)
    (add-hook 'org-mode-hook 'yxl-org/org-mode-hook)
    (add-hook 'org-mode-hook 'yxl-org/load-ob-helper))
  ;; inject my own configs
  (with-eval-after-load 'org
    (yxl-org//setup-general)
    (yxl-org//setup-keybindings)
    (yxl-org//setup-leader-keys)
    (yxl-org//setup-hydra)
    (add-hook 'org-font-lock-set-keywords-hook
              #'yxl-org//setup-extra-fontlock)
    (yxl-org//setup-latex)
    (yxl-org//setup-babel)
    (yxl-org//setup-minor-modes))
  (with-eval-after-load 'org-agenda
    (yxl-org//setup-agenda)))

(defun yxl-org/init-yxl-org ()
  (use-package yxl-org
    :after (org)
    :config
    (progn
      (setq yxl-org-task-files org-agenda-files))))

(defun yxl-org/init-org-goodies ()
  (use-package org-goodies
    :after (org)))

(defun yxl-org/init-org-babel-goodies ()
  (use-package org-babel-goodies
    :after (org)))

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

(defun yxl-org/init-org-recipes ()
  (use-package org-recipes
    :defer t
    :commands (org-recipes org-recipes-dwim)
    :init
    (progn
      (spacemacs/set-leader-keys "aor" #'org-recipes))
    :config
    (progn
      (require 'helm)
      (defun org-recipes-goto-recipe ()
        (interactive)
        (with-helm-alive-p
          (helm-exit-and-execute-action #'org-recipes--persistent-view)))
      (defun org-recipes-goto-recipe-below ()
        (interactive)
        (let ((f (lambda (c)
                   (split-window-below-and-focus)
                   (find-file (org-recipes--get-file c))
                   (goto-line (org-recipes--get-line c))
                   (org-show-subtree))))
          (with-helm-alive-p
            (helm-exit-and-execute-action f))))
      (define-key org-recipes-map
        (kbd "C-u") #'org-recipes-goto-recipe)
      (define-key org-recipes-map
        (kbd "C-o") #'org-recipes-goto-recipe-below)
      (define-key org-recipes-map
        (kbd "RET") #'org-recipes-insert))))
