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
(setq yxl-path-journal-more-pwd (concat yxl-path-sync "journal_yxl"))

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

(setq yxl-file-org-todo (concat yxl-path-org-task "todo.org"))
(setq yxl-file-org-log (concat yxl-path-org "logs/"))
(setq yxl-file-org-scratch (concat yxl-path-org-task "scratch.org"))
(setq yxl-file-org-local "~/scratch-local.org")

(setq yxl-env-org-task-files (list yxl-file-org-scratch
                                   yxl-file-org-todo))

(defun yxl-env-project-view (&optional same-frame side-width)
  (interactive)
  (unless same-frame
    (make-frame))
  (delete-other-windows)
  ;; open main org files
  (yxl-find-file-open-all yxl-env-project-files)
  ;; open scratch as sidebar
  (yxl-find-file-popup yxl-file-org-scratch side-width)
  (split-window-below-and-focus)
  (find-file yxl-file-org-local)
  (evil-window-right 1))

(defun yxl-env-update-org-files ()
  ;; update org-related variables (and variables dependent on them) that are
  ;; dynamically set.
  (interactive)
  (setq yxl-env-project-files
        (append
         (directory-files (concat yxl-path-org "projects/")
                          t "^proj_.+\\.org")
         ;; only find files that begins with "log_"
         (directory-files (concat yxl-path-org "logs/")
                          t "^log_.+\\.org")))
  (setq yxl-env-org-files (append yxl-env-org-task-files
                                  yxl-env-project-files))
  (setq org-agenda-files yxl-env-org-files)
  (setq yxl-hhs-org-files yxl-env-org-files))

(yxl-env-update-org-files)
