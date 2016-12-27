(require 'helm)



(setq yxl-helm-quick--fallback
      (helm-build-sync-source "Helm Quick"
        :match (lambda (_candidate) t)  ;; persistent
        :candidates '(("yxl-helm-quick" . yxl-helm-quick))
        :action (lambda (candidate) (funcall candidate))))

(defun yxl-helm-org-files ()
  (interactive)
  (helm :sources '(((name . "Org agenda")
                   (candidates . yxl-org-files)
                   (action . (("open" . find-file)
                              ("open other window" . find-file-other-window))))
                   yxl-helm-quick--fallback)
        :buffer "*helm org agenda*"))

(defun yxl-helm-set-simple-todo ()
  (interactive)
  (helm :sources '(((name . "Simple todo")
                   (candidates . (("task1" . yxl-set-simple-todo-task1)
                                  ("task2" . yxl-set-simple-todo-task2)
                                  ("task3" . yxl-set-simple-todo-task3)))
                   (action . (("open" . (lambda (x) (call-interactively x))))))
                   yxl-helm-quick--fallback)
        :buffer "*helm yxl simple-todo*"))

(defun yxl-helm-websites ()
  (interactive)
  (helm :sources '(((name . "Websites")
                   (candidates . yxl-env-websites-alist)
                   (action . (("open" . (lambda (x) (browse-url-generic x))))))
                   yxl-helm-websites--fallback)))

(setq yxl-helm-websites--fallback
      (helm-build-sync-source "Fallback"
        :match (lambda (_candidate) t)
        :candidates '(("yxl-helm-quick" . (lambda (x) (yxl-helm-quick)))
                      ("Google search" . (lambda (x)
                                           (let* ((google-base "http://www.google.com/search?q=%s")
                                                  (query-string (replace-regexp-in-string " " "\+" x))
                                                  (url-string (format google-base query-string)))
                                             (browse-url-generic url-string))))
                      ("Direct Input" . (lambda (x) (browse-url-generic x))))
        :action (lambda (candidate) (funcall candidate helm-pattern))))

(defun yxl-helm-files ()
  (interactive)
  (helm :sources '(((name . "Files and Directories")
                   (candidates . yxl-env-files-alist)
                   (action . (("open" . (lambda (x) (find-file x))))))
                   yxl-helm-quick--fallback)))

(defun yxl-helm-quick ()
  (interactive)
  (helm :sources '((name . "yxl quick")
                   (candidates . (
                                  ;; ("simple-todo" . yxl-helm-set-simple-todo)
                                  ("yxl-helm-org-files" . yxl-helm-org-files)
                                  ("yxl-helm-files" . yxl-helm-files)
                                  ("yxl-helm-websites" . yxl-helm-websites)
                                  ("calendar" . cfw-open-calendar)
                                  ("calculator" . (lambda ()
                                                    (helm-calcul-expression)))
                                  ("rss" . elfeed)
                                  ("helm-github-stars" . helm-github-stars)
                                  ("helm-show-kill-ring" . helm-show-kill-ring)
                                  ("helm-all-mark-rings" . helm-all-mark-rings)))
                   (action . (("open" . (lambda (x) (funcall x))))))
        :buffer "*helm yxl quick*"))



(provide 'yxl-helm-quick)
