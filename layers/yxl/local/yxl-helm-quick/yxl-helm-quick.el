(require 'helm)



(setq yxl-helm-quick--fallback
      (helm-build-sync-source "Helm Quick"
        :match (lambda (_candidate) t)  ;; persistent
        :candidates '(("yxl-helm-quick" . yxl-helm-quick))
        :action (lambda (candidate) (funcall candidate))))

;; deprecated
;; (defun yxl-helm-set-simple-todo ()
;;   (interactive)
;;   (helm :sources '(((name . "Simple todo")
;;                    (candidates . (("task1" . yxl-set-simple-todo-task1)
;;                                   ("task2" . yxl-set-simple-todo-task2)
;;                                   ("task3" . yxl-set-simple-todo-task3)))
;;                    (action . (("open" . (lambda (x) (call-interactively x))))))
;;                    yxl-helm-quick--fallback)
;;         :buffer "*helm yxl simple-todo*"))


(defun yxl-helm-org-files ()
  (interactive)
  (helm :sources
        `(,(helm-build-sync-source "Org files"
             :candidates yxl-env-org-files
             :action  (helm-make-actions
                       "open" #'find-file
                       "open other window" #'find-file-other-window))
          ,(helm-build-sync-source "Fallback"
             :match (lambda (_candidate) t)
             :candidates '(("open all task files" .
                            (lambda (x)
                              (yxl-find-file-open-all yxl-env-org-task-files)))
                           ("open all org files" .
                            (lambda (x)
                              (yxl-find-file-open-all yxl-env-org-files))))
             :action (lambda (candidate) (funcall candidate helm-pattern))))
        :buffer "*helm org agenda*"))

(defun yxl-helm-websites ()
  (interactive)
  (helm :sources
        `(,(helm-build-sync-source "Websites"
             :candidates yxl-env-websites-alist
             :action (helm-make-actions
                      "open" (lambda (x) (browse-url-generic x))
                      "open-w3m" (lambda (x) (w3m-goto-url-new-session x))))
          ,(helm-build-sync-source "Fallback"
             :match (lambda (_candidate) t)
             :candidates '(("yxl-helm-quick" . (lambda (x) (yxl-helm-quick)))
                           ("Google search" .
                            (lambda (x)
                              (let* ((google-base "http://www.google.com/search?q=%s")
                                     (query-string (replace-regexp-in-string " " "\+" x))
                                     (url-string (format google-base query-string)))
                                (browse-url-generic url-string))))
                           ("Direct Input" . (lambda (x) (browse-url-generic x))))
             :action (lambda (candidate) (funcall candidate helm-pattern))))))

(defun yxl-helm-files ()
  (interactive)
  (helm :sources '(((name . "Files and Directories")
                   (candidates . yxl-env-files-alist)
                   (action . (("open" . (lambda (x) (find-file x))))))
                   yxl-helm-quick--fallback)))

(defun yxl-helm-quick ()
  (interactive)
  (helm :sources '((name . "yxl quick")
                   (candidates . (("yxl-helm-org-files" . yxl-helm-org-files)
                                  ;; ("simple-todo" . yxl-helm-set-simple-todo)
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
