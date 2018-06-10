(require 'general)

;; TODO:
;; - Simplify these
;; --------
;; Personal variable configs
;; --------
(setq yxl-base-org-directory "~/Dropbox/org/")
(with-eval-after-load 'org
  (setq org-directory yxl-base-org-directory))
(setq yxl-base-org-todo-work (concat yxl-base-org-directory "tasks/todo-work.org"))
(setq yxl-base-org-log (concat yxl-base-org-directory "logs/logs-master.org"))
(setq yxl-base-org-todo-life (concat yxl-base-org-directory "tasks/todo-life.org"))
(setq yxl-base-org-calendar (concat yxl-base-org-directory "tasks/calendar.org"))
(setq yxl-base-note-local "~/local-repo/local-notes.org")
(setq yxl-base-note-sync "~/Dropbox/org/note.org")

;; org-agenda-files
(setq org-agenda-files nil)
(mapc (lambda (elem)
        (add-to-list 'org-agenda-files (expand-file-name elem) t))
      (append
       (list yxl-base-org-todo-life yxl-base-org-todo-work yxl-base-org-calendar)
       (directory-files (concat yxl-base-org-directory "projects/")
                        t "^proj_.+\\.org")
       (directory-files (concat yxl-base-org-directory "logs/")
                        t "^log_.+\\.org")
       (directory-files "~/local-repo" t ".org")))
(setq ivy-todo-file "~/Dropbox/org/tasks/todo.org")
;; HACK workaround a bug in org-recipes
(setq org-wiki-location
      (expand-file-name (concat yxl-base-org-directory)))
(setq org-recipes-file-list
      (directory-files (expand-file-name (concat yxl-base-org-directory
                                                 "recipes/"))
                       t "^.+\\.org"))
;; org capture template
(setq org-capture-templates
      '(("c" "today: today" entry
         (file+headline yxl-base-org-todo-life "Today")
         "** TODO %?\n")
        ("t" "today: later" entry
         (file+headline yxl-base-org-todo-life "Later")
         "** %?\n")
        ("i" "general: inbox" checkitem
         (file+headline yxl-base-org-todo-work "Capture")
         "** %?\n  %i\n")
        ("k" "calendar: today" entry
         (file+headline yxl-base-org-calendar "Items")
         "** %? <%(org-read-date nil nil \"+1d\")>\n")))
(with-eval-after-load 'calfw-org
  (add-to-list 'org-capture-templates
               '("K" "calendar: cfw" entry
                 (file+headline yxl-base-org-calendar "Items")
                 "** %? <%(cfw/most-recent-date)>\n")
               t))

(setq org-refile-targets '((nil :maxlevel . 1)
                           (yxl-base-org-task-files :maxlevel . 1)))

(setq yxl-ivy-views-storage-location "~/Dropbox/inbox/yxl-ivy-views.txt")
(setq yxl-hhs-org-files org-agenda-files)
(setq yxl-hhs-file-local-list "~/Dropbox/inbox/yxl-sites-local.txt")
(setq yxl-hhs-file-web-list "~/Dropbox/inbox/yxl-sites-web.txt")
(setq yxl-hhs-file-reading-list-local
      "~/Dropbox/inbox/yxl-reading-list-files.txt")
(setq yxl-hhs-file-reading-list-webpages
      "~/Dropbox/inbox/yxl-reading-list-webpages.txt")
(setq helm-github-stars-username "YiLiu6240")
(setq yxl-ia-list '(("calendar" . calendar)
                    ("counsel-books" . counsel-books)
                    ("hackernews" . hackernews)
                    ("org-agenda" . org-agenda)
                    ("org-capture" . org-capture)
                    ("w3m" . w3m)
                    ("cfw/open-calendar" . cfw/open-calendar)
                    ("rss: elfeed" . elfeed)
                    ("note: deft" . spacemacs/deft)
                    ("email: mu4e" . mu4e)
                    ("counsel-dash" . counsel-dash)
                    ("helm-github-stars" . helm-github-stars)
                    ("helm-chrome-bookmarks" . helm-chrome-bookmarks)
                    ("helm-bibtex" . helm-bibtex)
                    ("gscholar-bibtex" . gscholar-bibtex)
                    ("helm-google-suggest" . helm-google-suggest)
                    ("academic-phrases" . academic-phrases)))
(setq org-ref-bibliography-notes "~/Dropbox/bib/bib_notes.org")
(setq bibtex-completion-notes-path "~/Dropbox/bib/bib_notes.org")
(setq biblio-download-directory "~/Dropbox/bib/general")
(setq bibtex-completion-library-path '("~/Dropbox/bib/general"
                                       "~/Dropbox/bib/topic_tmp"))
(setq gscholar-bibtex-database-file yxl-base-bib)
(setq-default bookmark-default-file "~/Dropbox/inbox/helm-bookmark")

;; --------
;; General
;; --------
;; emacs general
(setq-default menu-bar-mode nil)
(setq scroll-margin 1)
(setq-default comment-add 0)
;; osx
(setq-default mac-option-modifier 'super
              mac-command-modifier 'meta)
(setq-default ns-use-srgb-colorspace t)
(setq-default initial-major-mode 'fundamental-mode)
;; ui
(setq-default neo-theme 'ascii)
(setq neo-show-updir-line t)
(setq-default fci-rule-color (face-attribute 'highlight :background))
(setq neo-window-width 20)
(global-hl-line-mode -1)

;; --------
;; evil escape
;; --------
(setq-default evil-escape-key-sequence "jk")
;; only use "jk" in insert state
(setq evil-escape-excluded-states '(visual
                                    evilified
                                    normal
                                    motion
                                    emacs
                                    replace
                                    hybrid
                                    lisp
                                    iedit
                                    iedit-insert))
(with-eval-after-load 'treemacs
  (add-to-list 'evil-escape-excluded-states 'treemacs))
(setq-default evil-escape-delay 1)
(setq-default evil-escape-excluded-major-modes '(magit-mode))

;; --------
;; yas
;; --------
(add-to-list 'yas-snippet-dirs "~/dotfiles/yasnippets")

;; indent
;; (setq-default tab-width 4)
(setq-default evil-shift-width 2)
(setq-default indent-tabs-mode nil)

;; --------
;; auto mode list
;; --------
(add-to-list 'auto-mode-alist '("\\.todo$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.inbox$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.tex$" . latex-mode))
(add-to-list 'auto-mode-alist '("\\.sublime-settings$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.sublime-keymap$" . json-mode))

;; display-buffer-alist
(add-to-list 'display-buffer-alist '("\\*R" display-buffer-same-window))
;; (add-to-list 'display-buffer-alist '("\\*R" display-buffer-pop-up-window))
;; FIXME: not working
(add-to-list 'display-buffer-alist '("\\*Python\\*" display-buffer-same-window))
;; (add-to-list 'display-buffer-alist '("\\*magit:" display-buffer-same-window))
(add-to-list 'display-buffer-alist '("\\*shell" display-buffer-same-window))
(add-to-list 'display-buffer-alist '("\\*PDF" display-buffer-at-bottom))
(add-to-list 'display-buffer-alist '("\\*edit-indirect" display-buffer-at-bottom))

(push '("^\*help.+\*$"
        :regexp t
        :dedicated t :position bottom :stick t :noselect t :height 0.4)
      popwin:special-display-config)

;; yas
;; solve an known issue revolving evil visual and yasnippet
;; TODO: check if actually need that hook
;; (with-eval-after-load 'yasnippet
;;   (add-hook 'snippet-mode-hook
;;             (lambda ()
;;               (setq mode-require-final-newline nil))))

;; spacemacs buffer
(add-hook 'spacemacs-buffer-mode-hook
          (lambda () (define-key spacemacs-buffer-mode-map
                       "o" 'widget-button-press)))

;; Boring/useful buffers
(let ((useless-buf '("*\.\+" "TAGS")))
  (mapc (lambda (elem) (add-to-list 'spacemacs-useless-buffers-regexp elem))
        useless-buf))
(let ((useful-buf '("\\*scratch\\*" "\\*Org")))
  (mapc (lambda (elem) (add-to-list 'spacemacs-useful-buffers-regexp elem))
        useful-buf))

;; proselint
(with-eval-after-load 'flycheck
  (flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message (one-or-more not-newline)
                       (zero-or-more "\n" (any " ") (one-or-more not-newline)))
              line-end))
    :modes (text-mode markdown-mode gfm-mode LaTeX-mode))
  (add-to-list 'flycheck-checkers 'proselint))

;; calendar
(setq calendar-week-start-day 1)

;; --------
;; origami
;; --------

(add-to-list 'origami-parser-alist
             `(conf-space-mode . ,(origami-markers-parser "{{{" "}}}")))
(add-to-list 'origami-parser-alist
             `(lua-mode . ,(origami-markers-parser "{{{" "}}}")))
(add-to-list 'origami-parser-alist
             `(bibtex-mode . ,(origami-markers-parser "{{{" "}}}")))
(setq-default origami-show-fold-header nil)

;; --------
;; imenu
;; --------
(setq imenu-list-position 'left)
(setq imenu-list-focus-after-activation nil)

;; --------
;; Bibliography
;; --------
(setq org-ref-default-bibliography yxl-base-bib)
(setq bibtex-completion-bibliography yxl-base-bib)

;; --------
;; IDE
;; --------
(setq yxl-dp-docs
  '(("R - quantreg" . "https://cran.r-project.org/web/packages/quantreg/quantreg.pdf")
    ("Moab" . "http://docs.adaptivecomputing.com/torque/6-1-1/adminGuide/help.htm#topics/moabWorkloadManager/topics/moabCommands/a.gcommandoverview.html")
    ("Slurm" . "https://slurm.schedmd.com/")
    ("Cpp - Armadillo" . "http://arma.sourceforge.net/docs.html")
    ("Python - strftime" . "http://strftime.org/")
    ("Python - string format" . "https://pyformat.info/")
    ("EIN" . "http://millejoh.github.io/emacs-ipython-notebook/")
    ("GFM" . "https://guides.github.com/features/mastering-markdown/")
    ("org-babel" . "http://orgmode.org/manual/Specific-header-arguments.html")
    ("R - adv-r" . "http://adv-r.had.co.nz/")
    ("R - rmarkdown - html" . "http://rmarkdown.rstudio.com/html_document_format.html")
    ("R - rmarkdown - cheatsheet" . "https://www.rstudio.com/wp-content/uploads/2015/03/rmarkdown-reference.pdf")
    ("R - knitr - options" . "https://yihui.name/knitr/options/")
    ("R - bookdown" . "https://bookdown.org/yihui/bookdown/")
    ("R - blogdown" . "https://bookdown.org/yihui/blogdown/")
    ("R - igraph" . "http://igraph.org/r/doc/00Index.html")
    ("R - cowplot" . "https://cran.r-project.org/web/packages/cowplot/")
    ("R - modelr" . "https://cran.r-project.org/web/packages/modelr/modelr.pdf")
    ("R - GGally" . "http://ggobi.github.io/ggally/")
    ("R - tidyverse - dplyr" . "http://dplyr.tidyverse.org/reference/index.html")
    ("R - tidyverse - stringr" . "http://stringr.tidyverse.org/reference/index.html")
    ("R - tidyverse - purrr" . "http://purrr.tidyverse.org/reference/index.html")
    ("R - tidyverse - rlang" . "http://rlang.tidyverse.org/")
    ("R - TraMineR" . "http://traminer.unige.ch/doc/00Index.html")
    ("R - stargazer" . "http://www.jakeruss.com/cheatsheets/stargazer/")
    ("Color - solarized" . "http://ethanschoonover.com/solarized")
    ("Color - gruvbox" . "https://github.com/morhetz/gruvbox")
    ("Color - material" . "https://material.io/guidelines/style/color.html#color-color-palette")
    ("Color - hexrgb" . "https://www.webpagefx.com/web-design/hex-to-rgb/")))

;; --------
;; Misc. configs
;; TODO: need to clean this up
;; --------
;; misc stuff
(setq-default require-final-newline t)
(setq-default auto-revert-interval 60)

;; tramp bug, from zilongshanren
(setq-default tramp-ssh-controlmaster-options
              "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

;; deal with html export problem in orgmode with fci
;; https://github.com/alpaker/Fill-Column-Indicator/issues/45
(defun fci-mode-override-advice (&rest args))
(advice-add 'org-html-fontify-code :around
            (lambda (fun &rest args)
              (advice-add 'fci-mode :override #'fci-mode-override-advice)
              (let ((result  (apply fun args)))
                (advice-remove 'fci-mode #'fci-mode-override-advice)
                result)))

;; override spacemacs popwin
(delete '("^\*WoMan.+\*$" :regexp t :position bottom)
        popwin:special-display-config)
;; (delete '("*Help*" :dedicated t :position bottom :stick t :noselect t :height 0.4)
;;       popwin:special-display-config)

;; force prefer-coding-system
(prefer-coding-system 'utf-8-unix)

;; (which-function-mode 1)
(setq which-func-unknown "--")

(evilified-state-evilify-map special-mode-map
  :mode special-mode)

;; ----------------
;; Last things to do
;; ----------------
(add-hook 'spacemacs-post-user-config-hook
          (lambda ()
            ;; add hook so that modeline colors are set correctly
            ;; after theme change
            (add-hook 'spacemacs-post-theme-change-hook
                      #'yxl-airline-theme-set-colors)
            (load-file (concat yxl-path-personal "personal-config.el"))
            ;; Override spacemacs home to be goto ~/Downloads/
            (advice-add 'spacemacs-buffer/goto-buffer
                        :override
                        (lambda (&optional refresh)
                          (interactive)
                          (find-file "~/Downloads/")))
            (yxl-spacemacs-dashboard))
          t)

(add-hook 'spacemacs-post-user-config-hook
          ;; Update any TODO.org in `yxl-base-freq-projects-alist'
          (lambda ()
            (mapc (lambda (alist)
                    (mapc (lambda (elem)
                            (add-to-list 'org-agenda-files elem t))
                          (directory-files (car alist) t "^TODO.org")))
                  yxl-base-freq-projects-alist)))
