(setq yxl-path-dotfiles "~/dotfiles/")
(setq yxl-path-sync "~/Dropbox/")  ;; sync location for repos
(setq yxl-path-local "~/local-repo/")  ;; local location for repos
(setq yxl-path-downloads "~/Downloads/")
(setq yxl-path-org "~/Dropbox/org/")
(setq yxl-path-org-task "~/Dropbox/org/tasks/")
(setq yxl-path-projects (concat yxl-path-local "yxl_projects"))

(setq yxl-path-code-repo (concat yxl-path-local "c3_pwd/"))
(setq yxl-path-phd-repo (concat yxl-path-local "yxl105_ctw/"))
(setq yxl-path-code-pwd (concat yxl-path-local "c3_pwd/code/"))
(setq yxl-path-paper-pwd (concat yxl-path-local "yxl105_ctw/yxl105_tex/"))
(setq yxl-path-journal-pwd (concat yxl-path-sync "journal_yxl"))

(setq yxl-path-book-reference (concat yxl-path-sync "book_reference"))

;; hydra
(with-eval-after-load 'hydra
  (defhydra yxl-find-dir (:color blue)
    "Directory: "
    ("d" (find-file yxl-path-dotfiles) "dotfiles")
    ("g" (find-file yxl-path-downloads) "downloads")
    ("G" (find-file yxl-path-local) "local-repo")
    ("h" (find-file yxl-path-sync) "dropbox")
    ("H" (find-file yxl-path-projects) "projects")
    ("o" (find-file yxl-path-org) "org")
    ("c" (find-file yxl-path-code-pwd) "code")
    ("p" (find-file yxl-path-paper-pwd) "papers")
    ("j" (find-file yxl-path-journal-pwd) "journals")
    ("b" (find-file yxl-path-book-reference) "books")))



(setq yxl-file-bib (concat yxl-path-sync "bib/yxl_bib_master.bib"))
(setq yxl-file-note-master "~/Dropbox/org/note.org")
(setq yxl-file-org-main (concat yxl-path-org-task "tasks_1_main.org"))
(setq yxl-file-org-work (concat yxl-path-org-task "tasks_2_work.org"))
(setq yxl-file-org-config (concat yxl-path-org-task "tasks_3_config.org"))
(setq yxl-file-org-proj (concat yxl-path-org-task "tasks_4_proj.org"))
(setq yxl-file-org-log (concat yxl-path-org "logs/" "log.org"))
(setq yxl-file-org-scratch (concat yxl-path-org "scratch.org"))

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

(with-eval-after-load 'hydra
  (defhydra yxl-find-file (:color blue)
    "File: "
    ("1" (yxl-find-file-popup yxl-file-org-main) "tasks_1_main.org")
    ("2" (yxl-find-file-popup yxl-file-org-work) "tasks_2_work.org")
    ("3" (yxl-find-file-popup yxl-file-org-config) "tasks_3_config.org")
    ("4" (yxl-find-file-popup yxl-file-org-proj) "tasks_4_proj.org")
    ("0" (yxl-find-file-popup yxl-file-org-scratch) "scratch.org")
    ("b" (yxl-find-file-popup yxl-file-bib) "bib file")
    ("n" (yxl-find-file-popup yxl-file-note-master) "note file")
    ("e" (yxl-find-file-popup "~/Dropbox/Inbox/scratch.el") "scratch.el")))



(with-eval-after-load 'hydra
  (defhydra yxl-hydra-hotspot (:color blue)
    "Hotspot: "
    ("h" (yxl-frame-select-or-set "Code") "Frame: Code")
    ("j" (yxl-frame-select-or-set "REPL") "Frame: REPL")
    ("k" (yxl-frame-select-or-set "Meta") "Frame: Meta")
    ("l" (yxl-frame-select-or-set "Conf") "Frame: Conf")
    ("o" (yxl-org-open-all-task-files) "org: open all tasks")
    ("a" (org-agenda-list) "org: agenda")
    ("1" (yxl-find-file-popup yxl-file-org-main) "org: tasks_1_main.org")
    ("2" (yxl-find-file-popup yxl-file-org-work) "org: tasks_2_work.org")
    ("3" (yxl-find-file-popup yxl-file-org-config) "org: tasks_3_config.org")
    ("4" (yxl-find-file-popup yxl-file-org-proj) "org: tasks_4_proj.org")
    ("0" (yxl-find-file-popup yxl-file-org-scratch) "org: scratch.org")))


;; These alist will be fed to helm

(setq yxl-env-files-alist `(("dotfiles/" . "~/dotfiles/")
                            ("emacs.d/" . "~/.emacs.d/")
                            ("local-repos/" . ,yxl-path-local)
                            ("dropbox" . ,yxl-path-sync)
                            ("projects" . ,yxl-path-projects)
                            ("org/" . ,yxl-path-org)
                            ("journal-papers/" . ,yxl-path-journal-pwd)
                            ("bib" . ,yxl-file-bib)))

(setq yxl-env-websites-alist '(("wunderlist" . "https://www.wunderlist.com")
                               ("github" . "https://www.github.com")
                               ("bham-portable" . "http://my.bham.ac.uk")
                               ("gmail" . "https://www.gmail.com")))

(setq yxl-env-elfeed-tag-alist '(("emacs" . ("emacs" t))
                                 ("blog" . ("blog" t))
                                 ("tech_blog" . ("tech_blog" t))
                                 ("tech_news" . ("tech_news" t))
                                 ("news" . ("news" t))
                                 ("econ_news" . ("econ_news" t))
                                 ("work" . ("work" t))
                                 ("datascience" . ("datascience" t))
                                 ("econ_sites" . ("econ_sites" t))
                                 ("star" . ("+star" nil))))

(setq yxl-env-helm-hotspot-alist '(("yxl-helm-org-files" . yxl-helm-org-files)
                                   ("yxl-helm-files" . yxl-helm-files)
                                   ("yxl-helm-websites" . yxl-helm-websites)
                                   ("calendar" . cfw-open-calendar)
                                   ("calculator" . (lambda ()
                                                     (helm-calcul-expression)))
                                   ("rss" . elfeed)
                                   ("helm-github-stars" . helm-github-stars)
                                   ("helm-show-kill-ring" . helm-show-kill-ring)
                                   ("helm-all-mark-rings" . helm-all-mark-rings)))
