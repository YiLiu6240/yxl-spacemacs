(setq yxl-path-dotfiles "~/dotfiles/")
(setq yxl-path-sync "~/Dropbox/")  ;; sync location for repos
(setq yxl-path-local "~/local-repo/")  ;; local location for repos
(setq yxl-path-personal "~/dotfiles/personal/")  ;; location for personal files
(setq yxl-path-downloads "~/Downloads/")
(setq yxl-path-org "~/Dropbox/org/")
(setq yxl-path-org-task "~/Dropbox/org/tasks/")
(setq yxl-path-projects (concat yxl-path-local "yxl_projects"))

(setq yxl-path-code-pwd (concat yxl-path-local "c3_pwd/"))
(setq yxl-path-code-master-pwd (concat yxl-path-local "yxl_ctw_code/"))
(setq yxl-path-paper-pwd (concat yxl-path-local "yxl_ctw_paper/"))
(setq yxl-path-journal-pwd (concat yxl-path-sync "journal_yxl/ctw"))

(setq yxl-path-book-reference (concat yxl-path-sync "books-text"))



(setq yxl-file-bib (concat yxl-path-paper-pwd "yxl_bib/yxl_bib_master.bib"))
(setq yxl-file-note-master "~/Dropbox/org/note.org")
(setq yxl-file-sites-local (concat yxl-path-sync
                                          "inbox/yxl-sites-local.txt"))
(setq yxl-file-sites-web (concat yxl-path-sync
                                 "inbox/yxl-sites-web.txt"))
(setq yxl-file-reading-list-files (concat yxl-path-sync
                                          "inbox/yxl-reading-list-files.txt"))
(setq yxl-file-reading-list-webpages (concat yxl-path-sync
                                             "inbox/yxl-reading-list-webpages.txt"))

(setq yxl-file-ivy-views (concat yxl-path-sync "inbox/yxl-ivy-views.txt"))

(setq yxl-file-org-main (concat yxl-path-org-task "tasks_1_main.org"))
(setq yxl-file-org-work (concat yxl-path-org-task "tasks_2_work.org"))
(setq yxl-file-org-config (concat yxl-path-org-task "tasks_3_config.org"))
(setq yxl-file-org-proj (concat yxl-path-org-task "tasks_4_proj.org"))
(setq yxl-file-org-log (concat yxl-path-org "logs/"))
(setq yxl-file-org-scratch (concat yxl-path-org "scratch.org"))
(setq yxl-file-org-local "~/scratch-local.org")

(setq yxl-env-org-task-files (list yxl-file-org-scratch
                                   yxl-file-org-main
                                   yxl-file-org-work
                                   yxl-file-org-config
                                   yxl-file-org-proj))

(setq yxl-env-org-files (append yxl-env-org-task-files
                                (list
                                 (concat yxl-path-org-task "cal_gen.org")
                                 (concat yxl-path-org-task "cal_google.org")
                                 (concat yxl-path-org-task "proj_ds.org")
                                 (concat yxl-path-org-task "proj_ctw.org")
                                 (concat yxl-path-org-task "proj_bham.org"))))
