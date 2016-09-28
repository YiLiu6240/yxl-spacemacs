;; directory
(setq yxl/dotfiles "~/dotfiles/")
(setq yxl/Dropbox "~/Dropbox/")
(setq yxl/Downloads "~/Downloads/")

;; files
(setq yxl/file-dot-inbox "~/Downloads/.inbox")
(setq yxl/file-task-today  "~/Dropbox/org/TODAY.org")
(setq yxl/file-task-scratch "~/Dropbox/org/scratch.org")
(setq yxl/file-task-project-collection "~/Dropbox/org/proj_CTW.org")
(setq yxl/task-project-meta "~/Downloads/c3/c3_pwd/tex/meta.org")
(setq yxl/file-note-master "~/Dropbox/org/note.org")

;; bread and butter
(setq yxl/code-repo "~/Downloads/c3_pwd/")
(setq yxl/phd-repo "~/Downloads/yxl105_ctw/")
(setq yxl/code-pwd "~/Downloads/c3_pwd/code/")
(setq yxl/paper-pwd "~/Downloads/yxl105_ctw/yxl105_tex/")
(setq yxl/journal-pwd "~/Dropbox/lit_1_yxl105/1_thesis/")
(setq yxl/file-bib
      (concat yxl/paper-pwd "yxl105_bib/yxl105_bib_master.bib"))

;; org
(setq yxl/org-directory "~/Dropbox/org/")
(setq yxl/org-file-master
      (concat yxl/org-directory "tasks/" "tasks_main.org"))
(setq yxl/org-file-work
      (concat yxl/org-directory "tasks/" "tasks_work.org"))
(setq yxl/org-dotfile-repo-todo-file
      "~/dotfiles/TODO.org")
(setq yxl/org-log-file
      (concat yxl/org-directory "logs/" "log.org"))

;; org agenda
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

;; project stuff
(defvar-local master-dir nil)
