(require 'helm)



(defun yxl-helm-org-files ()
  (interactive)
  (helm :sources '((name . "Org agenda")
                   (candidates . yxl-org-files)
                   (action . (("open" . find-file)
                              ("open other window" . find-file-other-window))))
        :buffer "*helm org agenda*"))

(defun yxl-helm-set-simple-todo ()
  (interactive)
  (helm :sources '((name . "Simple todo")
                   (candidates . (("task1" . yxl-set-simple-todo-task1)
                                  ("task2" . yxl-set-simple-todo-task2)
                                  ("task3" . yxl-set-simple-todo-task3)))
                   (action . (("open" . (lambda (x) (call-interactively x))))))
        :buffer "*helm yxl simple-todo*"))

(defun yxl-helm-websites ()
  (interactive)
  (helm :sources '((name . "Websites")
                   (candidates . yxl-env-files-alist)
                   (action . (("open" . (lambda (x) (browse-url-generic x))))))))

(defun yxl-helm-files ()
  (interactive)
  (helm :sources '((name . "Files and Directories")
                   (candidates . yxl-env-websites-alist)
                   (action . (("open" . (lambda (x) (find-file x))))))))

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
