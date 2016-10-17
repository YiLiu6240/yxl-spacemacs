;; directory
(defvar yxl/dotfiles "~/dotfiles/")
(defvar yxl/Dropbox "~/Dropbox/")
(defvar yxl/Downloads "~/Downloads/")

;; files
(defvar yxl/file-dot-inbox "~/Downloads/.inbox")
(defvar yxl/file-task-today  "~/Dropbox/org/TODAY.org")
(defvar yxl/file-task-scratch "~/Dropbox/org/scratch.org")
(defvar yxl/file-task-project-collection "~/Dropbox/org/proj_CTW.org")
(defvar yxl/task-project-meta "~/Downloads/c3/c3_pwd/tex/meta.org")
(defvar yxl/file-note-master "~/Dropbox/org/note.org")

;; bread and butter
(defvar yxl/code-repo "~/Downloads/c3_pwd/")
(defvar yxl/phd-repo "~/Downloads/yxl105_ctw/")
(defvar yxl/code-pwd "~/Downloads/c3_pwd/code/")
(defvar yxl/paper-pwd "~/Downloads/yxl105_ctw/yxl105_tex/")
(defvar yxl/journal-pwd "~/Dropbox/lit_1_yxl105/1_thesis/")
(defvar yxl/file-bib
  (concat yxl/paper-pwd "yxl105_bib/yxl105_bib_master.bib"))

;; org
(defvar yxl/org-directory "~/Dropbox/org/")
(defvar yxl/org-file-master
  (concat yxl/org-directory "tasks/" "tasks_main.org"))
(defvar yxl/org-file-work
  (concat yxl/org-directory "tasks/" "tasks_work.org"))
(defvar yxl/org-dotfile-repo-todo-file
  "~/dotfiles/TODO.org")
(defvar yxl/org-log-file
  (concat yxl/org-directory "logs/" "log.org"))

;; org agenda
(defvar yxl/org-agenda-files-life (list yxl/org-file-master
                                        yxl/org-dotfile-repo-todo-file))
(defvar yxl/org-agenda-files-work
  (list yxl/org-file-work
        (concat yxl/org-directory "tasks/" "proj_ctw.org")
        (concat yxl/org-directory "tasks/" "proj_bham.org")))
(defvar yxl/org-agenda-files (append yxl/org-agenda-files-life
                                     yxl/org-agenda-files-work))
;; project stuff
(defvar-local master-dir nil)

(push '(width . 84) initial-frame-alist)
(push '(height . 60) initial-frame-alist)
(push '(width . 84) default-frame-alist)
(push '(height . 60) default-frame-alist)
