(setq helm-mini-default-sources '(helm-source-buffers-list))

(setq yxl-helm-org-sources
      '((name . "org agenda")
        (candidates . yxl-org-files)
        (action . (("open" . find-file)
                   ("open other window" . find-file-other-window)))))

(setq yxl-helm-simple-todo-sources
      '((name . "simple todo")
        (candidates . (("task1" . yxl-set-simple-todo-task1)
                       ("task2" . yxl-set-simple-todo-task2)
                       ("task3" . yxl-set-simple-todo-task3)))
        (action . (("open" . (lambda (x) (call-interactively x)))))))

(setq yxl-helm-quick-sources
      '((name . "yxl quick")
        (candidates . (("simple-todo" . yxl-helm-set-simple-todo)
                       ("org-files" . yxl-helm-find-org-files)
                       ("calendar" . cfw-open-calendar)
                       ("calculator" . (lambda () (helm-calcul-expression)))
                       ("rss" . elfeed)))
        (action . (("open" . (lambda (x) (funcall x)))))))
