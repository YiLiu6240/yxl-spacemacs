(setq helm-mini-default-sources '(helm-source-buffers-list))

(setq yxl-helm-org-agenda-sources
      '((name . "org agenda")
        (candidates . org-agenda-files)
        (action . (("open" . find-file)
                   ("open other window" . find-file-other-window)))))

(setq yxl-helm-simple-todo-sources
      '((name . "simple todo")
        (candidates . (("task1" . yxl/set-simple-todo-task1)
                       ("task2" . yxl/set-simple-todo-task2)
                       ("task3" . yxl/set-simple-todo-task3)))
        (action . (("open" . (lambda (x) (call-interactively x)))))))

(setq yxl-helm-quick-sources
      '((name . "yxl quick")
        (candidates . (("simple-todo" . yxl/helm-set-simple-todo)
                       ("agenda" . yxl/helm-find-org-agenda)
                       ("calendar" . cfw-open-calendar)
                       ("calculator" . (lambda () (helm-calcul-expression)))
                       ("rss" . elfeed)))
        (action . (("open" . (lambda (x) (funcall x)))))))

(setq yxl-helm-pdf-occur-sources-preset
      '((name . "Preset Keywords")
        (candidates . yxl-pdf-occur-preset)
        (action . (("Search" . (lambda (x)
                                 (interactive)
                                 (pdf-occur x t)))))))

(setq yxl-helm-pdf-occur-sources-all
      `((name . "All Keywords")
        (candidates . (("Search all" . yxl/pdf-occur-search-preset)))
        (action . (("open" . (lambda (x) (call-interactively x)))))))

(with-eval-after-load 'helm
  (setq yxl-helm-pdf-occur-sources-fallback
        (helm-build-sync-source "Fallback"
          :match (lambda (_candidate) t)
          :candidates '("Search Pattern")
          :action (lambda (cand)
                    (pdf-occur helm-pattern t)))))
