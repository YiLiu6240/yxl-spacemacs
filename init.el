;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs-base
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-additional-packages '(toml-mode)
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages
   `(persp-mode
     smooth-scrolling
     spaceline
     window-numbering
     winum
     org-bullets
     tern
     ess-smart-equals
     wolfram-mode
     vi-tilde-fringe
     open-junk-file
     neotree)
   dotspacemacs-install-packages 'used-but-keep-unused
   dotspacemacs-configuration-layers
   `(
     ;; NOTE: These layers are not loaded
     ;;       - spacemacs-layouts (persp, eyebrowse)
     ;;       NOTE: counsel-projectile is loaded by spacemacs-layouts
     ;;             which should not have been
     ;;       - spacemacs-modeline (spaceline)
     ;;       MAYBE: enable spacemacs purpose
     ;;       - spacemacs-purpose
     spacemacs-completion
     spacemacs-defaults
     spacemacs-editing-visual
     spacemacs-editing
     spacemacs-evil
     spacemacs-language
     spacemacs-misc
     spacemacs-navigation
     spacemacs-org
     spacemacs-project
     ;; spacemacs-purpose
     spacemacs-visual

     ;; prog langs
     emacs-lisp
     ;; ,(unless (spacemacs/system-is-mswindows)
     ;;    'lsp)
     python
     vimscript
     yaml
     windows-scripts
     autohotkey
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode)
     sql
     lua
     clojure
     scala
     javascript
     typescript
     html
     graphviz
     csv
     (shell :variables
            shell-default-shell (if (eq window-system 'w32) 'eshell
                                  'shell)
            shell-default-height 60
            shell-default-position 'bottom
            shell-default-full-span t)
     shell-scripts

     ;; note takings
     (deft :variables deft-directory "~/Dropbox/org/notes")
     (org :packages (not ox-reveal)
          :variables
          org-enable-reveal-js-support t)

     ;; markup langs
     ;; markdown: need vmd from npm: npm install -g vmd
     (markdown :packages (not mmm-mode)
               :variables markdown-live-preview-engine 'vmd)
     (pandoc :packages (not ox-pandoc))
     (latex :variables
            latex-build-command "LatexMk"
            latex-enable-auto-fill nil
            latex-enable-folding t
            ;; Enable magic-latex-buffer
            latex-enable-magic nil)
     bibtex

     ;; editing
     (vinegar :packages (not dired))
     parinfer

     ;; major util modes
     (spell-checking :variables
                     spell-checking-enable-by-default nil)
     (syntax-checking :variables
                      syntax-checking-enable-by-default nil)
     (helm :can-shadow nil)
     (ivy :variables
          ;; ivy-rich is slow
          ivy-enable-advanced-buffer-information nil)
     (auto-completion :variables
                      ;; use tab to complete
                      ;; return key enters new line
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-enable-sort-by-usage nil
                      auto-completion-enable-help-tooltip 'manual
                      :disabled-for     ; layer name
                      org
                      markdown)
     version-control
     git
     ,(unless (spacemacs/system-is-mswindows)
        'github)
     (treemacs :variables
               treemacs-use-git-mode nil)
     docker

     ;; minor utils modes
     colors
     (ibuffer :variables
              ;; dont set to projects, really freaking slow
              ibuffer-group-buffers-by 'modes)
     search-engine
     better-defaults
     imenu-list

     ;; fun
     (twitter :variables
              twittering-use-master-password t)

     ;; my private layers
     ;; +3rd-parties
     calfw
     ov-highlighter
     ;; +utils
     yxl-base
     ,(unless (spacemacs/system-is-mswindows)
        'yxl-git)
     yxl-pdf-tools
     yxl-utils
     yxl-dired
     yxl-general-config
     (yxl-completion)
     yxl-project
     yxl-misc
     ;; +edit
     yxl-edit
     yxl-evil
     ;; ui
     yxl-workspace
     yxl-ui
     ;; +lang
     yxl-prog
     yxl-text
     (yxl-ess :variables
              yxl-ess-enable-lsp nil)
     ;; yxl-jupyter
     (yxl-docs :variables
               yxl-docs-docset-path "~/Dropbox/dash-docsets")
     (yxl-org :variables
              yxl-org-babel-languages '((dot . t)
                                        (latex .t)
                                        (python .t)
                                        (shell .t)
                                        (R .t)
                                        (ipython . t)
                                        (scala . t)))
     ;; +webservice
     yxl-web
     ,(unless (spacemacs/system-is-mswindows)
        `(yxl-email :packages
                    (not mu4e-alert)
                    :variables
                    mu4e-installation-path "/usr/local/share/emacs/site-lisp/mu4e"))
     (yxl-elfeed :variables
                 elfeed-enable-web-interface t
                 yxl-elfeed-db-directory "~/Dropbox/rss/.elfeed"))))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-gc-cons '(100000000 0.1)
   dotspacemacs-use-spacelpa nil
   dotspacemacs-verify-spacelpa-archives nil
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory 'emacs-version
   dotspacemacs-editing-style '(vim :variables
                                    vim-style-visual-feedback nil
                                    vim-style-remap-Y-to-y$ t
                                    vim-style-retain-visual-state-on-shift t
                                    vim-style-visual-line-move-text nil
                                    vim-style-ex-substitute-global nil)
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 0
   dotspacemacs-startup-lists '((projects . 5)
                                (recents . 10))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'fundamental-mode
   dotspacemacs-initial-scratch-message nil
   dotspacemacs-themes '((yxl-gruv-dark :location site)
                         (yxl-gruv-light :location site)
                         (yxl-solar-dark :location site)
                         sanityinc-tomorrow-night)
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font `(,(cond ((eq system-type 'windows-nt)
                                       "Sarasa Mono Sc")
                                      ((eq system-type 'darwin)
                                       "hack")
                                      (t "Sarasa Mono Sc"))
                               :size ,(cond ((eq system-type 'darwin) 12)
                                            ((eq system-type 'windows-nt) 30)
                                            (t 14))
                               :weight normal
                               :width normal
                               :powerline-scale 1.3)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab t
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-auto-generate-layout-names nil
   dotspacemacs-large-file-size 100
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-switch-to-buffer-prefers-purpose nil
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'origami
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'current
   dotspacemacs-enable-server t
   dotspacemacs-persistent-server nil
   ;; MAYBE: favor rg when we are sufficiently confident in it
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   dotspacemacs-frame-title-format "%I: %b"
   dotspacemacs-icon-title-format nil
   dotspacemacs-whitespace-cleanup 'trailing
   dotspacemacs-zone-out-when-idle nil
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-init ()
  (setq yxl-path-dotfiles "~/dotfiles/")
  (setq yxl-path-personal "~/dotfiles/personal/")
  (setq custom-file (expand-file-name "custom.el" dotspacemacs-directory))
  ;; load path
  ;; Project structure:
  ;; - config: ideally only configuration code, i.e. no defun
  ;; - lisp: self funcs and routines, global to the project
  ;; - site-lisp: 3rd-party original / self-modified modules
  ;; - theme: colorschemes and ui stuff
  (dolist (path
           `(,(concat dotspacemacs-directory "config")
             ,(concat dotspacemacs-directory "lisp")
             ,(concat dotspacemacs-directory "site-lisp")
             ,(concat dotspacemacs-directory "theme/yxl-theme")
             ,(concat dotspacemacs-directory "theme/yxl-airline")
             ,(concat dotspacemacs-directory "theme/yxl-airline-theme")
             ,(concat yxl-path-dotfiles "yxl-emacs-goodies")
             ,(concat yxl-path-dotfiles "yxl-emacs-datascience-goodies")))
    (add-to-list 'load-path path))
  (dolist (path
           `(,(concat dotspacemacs-directory "theme/yxl-theme")
             ,(concat dotspacemacs-directory "theme/yxl-airline-theme")))
    (add-to-list 'custom-theme-load-path path))
  ;; init stage config
  (load-file (concat dotspacemacs-directory "config/config-init.el"))
  (load-file (concat yxl-path-personal "personal-init.el"))
  (load custom-file 'no-error 'no-message))

(defun dotspacemacs/user-config ()
  (load-theme 'yxl-airline t)
  (load-file (concat dotspacemacs-directory "config/config-post-init.el"))
  (load-file (concat dotspacemacs-directory "config/yxl-global-keybindings.el")))

(defun dotspacemacs/emacs-custom-settings ())
