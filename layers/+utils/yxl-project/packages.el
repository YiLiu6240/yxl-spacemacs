(setq yxl-project-packages '((yxl-project :location site)
                             projectile))

(defun yxl-project/init-yxl-project ()
  (use-package yxl-project
    :commands (yxl-project-helm
               yxl-project-shell-popup
               yxl-project-cite
               yxl-project-select
               yxl-project-popup)))

(defun yxl-project/post-init-projectile ()
  (with-eval-after-load 'projectile
    ;; inherit from zilongshanren
    (evil-set-initial-state 'occur-mode 'evilified)
    (add-to-list 'projectile-globally-ignored-file-suffixes ".html")
    (add-to-list 'projectile-globally-ignored-files "*.html")
    (setq projectile-tags-command
          "rg --files | ctags -Re --links=no -f \"%s\" %s -L -")
    (defun my/todo-occur ()
      (interactive)
      (if (projectile-project-p)
          (multi-occur (projectile-project-buffers) hl-todo-regexp)
        (occur my-todo-occur-regex)))
    (spacemacs/declare-prefix "p/" "TODO-occur")
    (spacemacs/set-leader-keys "p/t" #'my/todo-occur)))
