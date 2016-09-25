(setq yxl/org-directory "~/Dropbox/org/")

(setq yxl/org-file-master
      (concat yxl/org-directory "tasks/" "tasks_main.org"))

(setq yxl/org-file-work
      (concat yxl/org-directory "tasks/" "tasks_work.org"))

(setq yxl/org-dotfile-repo-todo-file
      "~/dotfiles/TODO.org")

(setq yxl/org-log-file
      (concat yxl/org-directory "logs/" "log.org"))

;; agenda vars ----
(setq yxl/org-agenda-files-life
      (list yxl/org-file-master
            yxl/org-dotfile-repo-todo-file))

(setq yxl/org-agenda-files-work
      (list yxl/org-file-work
            (concat yxl/org-directory "tasks/" "proj_ctw.org")
            (concat yxl/org-directory "tasks/" "proj_bham.org")))

(setq yxl/org-agenda-files
      (append yxl/org-agenda-files-life
              yxl/org-agenda-files-work))
;; ----
