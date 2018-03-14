;; --------
;; Personal variable configs
;; --------
(setq yxl-env-org-todo (concat org-directory "tasks/todo.org"))
(setq yxl-env-org-log (concat org-directory "logs/logs-master.org"))
(setq yxl-env-org-checkbox (concat "~/Dropbox/org/tasks/" "checkbox.org"))
(setq yxl-env-note-local "~/local-repo/local-notes.org")
(setq yxl-env-note-sync "~/Dropbox/org/note.org")

(setq org-agenda-files
      (append
       (list yxl-env-org-checkbox yxl-env-org-todo)
       (directory-files (concat org-directory "projects/")
                        t "^proj_.+\\.org")
       (directory-files (concat org-directory "logs/")
                        t "^log_.+\\.org")
       (directory-files "~/local-repo" t ".org")))

(setq ivy-todo-file "~/Dropbox/org/tasks/todo.org")
;; HACK workaround a bug in org-recipes
(setq org-wiki-location
      (expand-file-name (concat org-directory
                                "recipes/")))
(setq org-recipes-file-list
      (directory-files (expand-file-name (concat org-directory
                                                 "recipes/"))
                       t "^.+\\.org"))
(setq org-capture-templates
      '(("c" "checkbox: inbox" checkitem (file+headline yxl-env-org-checkbox "Checkbox")
         "-  %?\n")
        ("t" "checkbox: todo" checkitem (file+headline yxl-env-org-checkbox "Checkbox")
         "- [ ]  %?\n")
        ("i" "general: inbox" entry (file+headline yxl-env-org-todo "Capture")
         "* INBOX %?\n  %i\n")))
(setq org-refile-targets '((nil :maxlevel . 1)
                           (yxl-env-org-task-files :maxlevel . 1)))

(setq yxl-ivy-views-storage-location "~/Dropbox/inbox/yxl-ivy-views.txt")

(setq helm-github-stars-username "YiLiu6240")
(setq yxl-hhs-org-files org-agenda-files)
(setq yxl-hhs-file-local-list "~/Dropbox/inbox/yxl-sites-local.txt")
(setq yxl-hhs-file-web-list "~/Dropbox/inbox/yxl-sites-web.txt")
(setq yxl-hhs-file-reading-list-local
      "~/Dropbox/inbox/yxl-reading-list-files.txt")
(setq yxl-hhs-file-reading-list-webpages
      "~/Dropbox/inbox/yxl-reading-list-webpages.txt")
(setq yxl-ia-list '(("calendar" . calendar)
                    ("counsel-books" . counsel-books)
                    ("hackernews" . hackernews)
                    ("org-agenda" . org-agenda)
                    ("org-capture" . org-capture)
                    ("w3m" . w3m)
                    ("cfw-calendar" . cfw-calendar)
                    ("rss: elfeed" . elfeed)
                    ("note: deft" . spacemacs/deft)
                    ("email: mu4e" . mu4e)
                    ("counsel-dash" . counsel-dash)
                    ("helm-github-stars" . helm-github-stars)
                    ("helm-chrome-bookmarks" . helm-chrome-bookmarks)
                    ("helm-bibtex" . helm-bibtex)
                    ("gscholar-bibtex" . gscholar-bibtex)
                    ("helm-google-suggest" . helm-google-suggest)))

(setq org-ref-bibliography-notes "~/Dropbox/bib/bib_notes.org")
(setq bibtex-completion-notes-path "~/Dropbox/bib/bib_notes.org")
(setq biblio-download-directory "~/Dropbox/bib/general")
(setq bibtex-completion-library-path '("~/Dropbox/bib/general"
                                       "~/Dropbox/bib/topic_tmp"))
(setq gscholar-bibtex-database-file yxl-env-bib)
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

;; --------
;; evil escape
;; --------
(setq-default evil-escape-key-sequence "jk")
;; only use "jk" in insert state
(setq-default evil-escape-excluded-states
              '(visual evilified
                normal motion
                emacs replace hybrid
                lisp iedit iedit-insert))
(setq-default evil-escape-delay 1)
(setq-default evil-escape-excluded-major-modes '(magit-mode))

;; --------
;; yas
;; --------
(add-to-list 'yas-snippet-dirs (expand-file-name "snippets" dotspacemacs-directory))

;; indent
(setq-default tab-width 4)
(setq-default evil-shift-width 4)
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
(setq org-ref-default-bibliography yxl-env-bib)
(setq bibtex-completion-bibliography yxl-env-bib)

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

;; force prefer-coding-system
(prefer-coding-system 'utf-8-unix)

;; (which-function-mode 1)
(setq which-func-unknown "--")

(evilified-state-evilify-map special-mode-map
  :mode special-mode)
