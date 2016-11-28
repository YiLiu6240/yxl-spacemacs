(defvar yxl-path-dotfiles "~/dotfiles/")
(defvar yxl-path-sync "~/Dropbox/")  ;; sync location for repos
(defvar yxl-path-local "~/local-repo/")  ;; local location for repos
(defvar yxl-path-downloads "~/Downloads/")
(defvar yxl-path-org "~/Dropbox/org/")
(defvar yxl-path-org-task "~/Dropbox/org/tasks/")

(defvar yxl-path-code-repo (concat yxl-path-local "c3_pwd/"))
(defvar yxl-path-phd-repo (concat yxl-path-local "yxl105_ctw/"))
(defvar yxl-path-code-pwd (concat yxl-path-local "c3_pwd/code/"))
(defvar yxl-path-paper-pwd (concat yxl-path-local "yxl105_ctw/yxl105_tex/"))
(defvar yxl-path-journal-pwd (concat yxl-path-sync "lit_1_yxl105/1_thesis/"))
(defvar yxl-file-bib (concat yxl-path-sync "bib/yxl_bib_master.bib"))

(defvar yxl-file-note-master "~/Dropbox/org/note.org")
(defvar yxl-file-org-main (concat yxl-path-org-task "tasks_1_main.org"))
(defvar yxl-file-org-work (concat yxl-path-org-task "tasks_2_work.org"))
(defvar yxl-file-org-config (concat yxl-path-org-task "tasks_3_config.org"))
(defvar yxl-file-org-proj (concat yxl-path-org-task "tasks_4_proj.org"))
(defvar yxl-file-org-log (concat yxl-path-org "logs/" "log.org"))
(defvar yxl-file-org-scratch (concat yxl-path-org "scratch.org"))

(defvar yxl-org-task-files (list yxl-file-org-scratch
                                 yxl-file-org-main
                                 yxl-file-org-work
                                 yxl-file-org-config
                                 yxl-file-org-proj))

(defvar yxl-org-files (list yxl-file-org-scratch
                            yxl-file-org-main
                            yxl-file-org-work
                            yxl-file-org-config
                            yxl-file-org-proj
                            (concat yxl-path-org-task "proj_ds.org")
                            (concat yxl-path-org-task "proj_ctw.org")
                            (concat yxl-path-org-task "proj_bham.org")))
