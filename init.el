;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(better-defaults

     ;; major langs --------
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode)
     emacs-lisp
     ess
     (git :variables
          git-magit-status-fullscreen t)
     lua
     markdown
     org
     python
     sql
     scala
     (latex :variables
            ;; LatexMK has to be enabled to let spc fetch auctex-latexmk
            ;; latex-build-command "LatexMk"
            ;; NOTE: could not get latexmk to work properly, use latex
            latex-build-command "LaTeX"
            latex-enable-auto-fill nil
            latex-enable-folding t)
     javascript
     html

     ;; minor langs --------
     ipython-notebook
     vimscript
     yaml
     extra-langs
     autohotkey
     ;; windows-scripts

     ;; major features --------
     ivy
     (auto-completion :variables
                      ;; use tab to complete
                      ;; return key enters new line
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-enable-sort-by-usage t
                      ;; completion tool tip, nil, buggy in evil
                      auto-completion-enable-help-tooltip nil
                      :disabled-for ; layer name
                      org
                      markdown)
     ;; semantic
     pandoc
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     (spell-checking :variables
                     spell-checking-enable-by-default nil)
     (syntax-checking :variables
                      syntax-checking-enable-by-default nil)
     ;; vim/evil mode --------
     (vinegar :packages (not dired))
     (evil-snipe :variables
                 ;; f/t not bound to current line
                 evil-snipe-enable-alternate-f-and-t-behaviors t)

     ;; other layers --------
     ;; smex
     colors
     (ibuffer :variables
              ibuffer-group-buffers-by 'projects)
     search-engine
     deft
     (elfeed :variables
             rmh-elfeed-org-files (list "~/dotfiles/rss/feeds.org"))

     ;; os-dependent --------
     ;; osx

     ;; private --------
     csv
     calfw
     graphviz
     pdf-tools
     imenu-list
     palette
     helm-dash
     ;; window-purpose

     yxl)
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(ereader)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(org-bullets
                                    ido
                                    vi-tilde-fringe)
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '((recents . 5)
                                bookmarks
                                (projects . 7))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'gfm-mode
   dotspacemacs-themes '(yxl-dark
                         spacemacs-dark
                         solarized-dark
                         monokai
                         spacemacs-light
                         solarized-light)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font `("Input Mono Narrow"
                               :size ,(if (eq system-type 'darwin) 12 12)
                               :weight normal
                               :width normal
                               :powerline-scale 1.3)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-emacs-command-key ":"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   ;; TODO: check
   dotspacemacs-large-file-size 10
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title nil
   dotspacemacs-show-transient-state-color-guide nil
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling nil
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   ;; TODO: have-a-look
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup `all
   ))

(defun dotspacemacs/user-init ()
  (add-to-list 'load-path "~/.spacemacs.d/config")
  (add-to-list 'load-path "~/.spacemacs.d/theme/yxl-theme/")

  (add-to-list 'custom-theme-load-path "~/.spacemacs.d/theme/yxl-theme/")

  (load-file (concat dotspacemacs-directory "config/spacemacs_home.el"))
  (load-file (concat dotspacemacs-directory "config/hack.el"))
  (load-file (concat dotspacemacs-directory "config/init-config.el"))
  )

(defun dotspacemacs/user-config ()
  ;; --------
  ;; general
  ;; --------
  (load-file (concat dotspacemacs-directory "config/yxl-keys.el"))
  (load-file (concat dotspacemacs-directory "config/post-init-config.el"))
  ;; emacs general
  ;; osx
  (setq-default mac-option-modifier 'super
                mac-command-modifier 'meta)
  (setq-default ns-use-srgb-colorspace t)

  ;; ui
  (setq neo-theme 'ascii
        fci-rule-color "#073642")

  ;; evil escape
  (setq-default evil-escape-key-sequence "jk")
  ;; only use "jk" in insert state
  (setq-default evil-escape-excluded-states '(visual
                                              evilified
                                              normal
                                              motion
                                              emacs
                                              replace
                                              hybrid
                                              lisp
                                              iedit
                                              iedit-insert))
  (setq-default evil-escape-delay 1)
  (setq-default evil-escape-excluded-major-modes '(magit-mode))

  ;; yas
  (setq yas-snippet-dirs (expand-file-name "snippets" dotspacemacs-directory))

  ;; ---- indent ----
  (setq evil-shift-width 4
        c-basic-offset 4
        lua-indent-level 4)

  ;; mode list
  (setq auto-mode-alist (cons '("\\.inbox$" . gfm-mode) auto-mode-alist))

  ;; -----------
  ;; ==== general packages below ====
  ;; -----------
  ;; TODO: write a function to search these keywords in
  ;; opened buffers / directory / project
  ;; TODO: put them in different category, then combine them together
  (setq hl-todo-keyword-faces '(("HOLD" . "#268bd2")
                                ("TODO" . "#cb4b16")
                                ("NEXT" . "#859900")
                                ("THEM" . "#859900")
                                ("PROG" . "#cb4b16")
                                ("ISSUE" . "#cb4b16")
                                ("DOING" . "#b58900")
                                ("OKAY" . "#859900")
                                ("DONT" . "#2aa198")
                                ("IDEA" . "#2aa198")
                                ("FAIL" . "#dc322f")
                                ("DOC" . "#dc322f")
                                ("DONE" . "#586e75")
                                ("NOTE" . "#859900")
                                ("REVIEW" . "#859900")
                                ("KLUDGE" . "#859900")
                                ("HACK" . "#268bd2")
                                ("PATCH" . "#268bd2")
                                ("FIXME" . "#b58900")
                                ("XXX" . "#b58900")
                                ("XXXX" . "#cb4b16")
                                ("???" . "#dc322f")))

  ;; --------
  ;; pdf-tools
  ;; --------
  (when (configuration-layer/layer-usedp 'pdf-tools)
    (if (eq system-type 'darwin)
        ;; osx setup of pdf-tools
        ;; http://emacs.stackexchange.com/questions/13314/install-pdf-tools-on-emacs-macosx
        (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")))

  ;; --------
  ;; yas
  ;; --------
  ;; solve an known issue revolving evil visual and yasnippet
  ;; TODO: check if actually need that hook
  (with-eval-after-load 'yasnippet
    (add-hook 'yas-before-expand-snippet-hook
              #'(lambda()
                  (when (evil-visual-state-p)
                    (let ((p (point))
                          (m (mark)))
                      (evil-insert-state)
                      (goto-char p)
                      (set-mark m))))))

  ;; ;; deft
  ;; (setq deft-extensions '("md" "txt" "org"))
  ;; (setq deft-directory "~/Dropbox/notes")

  ;; spacemacs buffer
  (add-hook
   'spacemacs-buffer-mode-hook
   (lambda ()
     (define-key spacemacs-buffer-mode-map "o" 'widget-button-press)))

  ;; --------
  ;; proselint
  ;; --------
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

  ;; --------
  ;; under experiment
  ;; --------
  ;; https://www.reddit.com/r/emacs/comments/54agp2/from_an_evil_perspective_how_to_efficiently_edit/
  (define-key emacs-lisp-mode-map (kbd "C-<tab>") 'sp-indent-adjust-sexp)
  (define-key emacs-lisp-mode-map (kbd "C-S-<tab>") 'sp-dedent-adjust-sexp)

  ;; --------
  ;; misc configs
  ;; --------
  (setq bookmark-default-file "~/Dropbox/inbox/helm-bookmark")

  (setq lua-indent-level 4)
  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil)

  ;; tramp bug, from zilongshanren
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

  ;; custom.el
  ;; stop package-selected-packages to be written to init.el
  ;; https://www.reddit.com/r/emacs/comments/53zpv9/how_do_i_get_emacs_to_stop_adding_custom_fields/
  ;; REVIEW
  (defun package--save-selected-packages (&rest opt) nil)
  (setq custom-file (expand-file-name "custom.el" dotspacemacs-directory))
  (load custom-file)

  ;; final steps
  (switch-to-buffer "*scratch*")
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
