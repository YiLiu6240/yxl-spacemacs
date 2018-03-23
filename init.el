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
   `(persp-mode smooth-scrolling spaceline
     window-numbering winum org-bullets
     tern ess-smart-equals wolfram-mode
     vi-tilde-fringe
     ,(when (spacemacs/system-is-mswindows)
        'projectile)
     ;; open-junk-file is buggy and we never use it
     ,(when (spacemacs/system-is-mswindows)
        'projectile)
     open-junk-file)
   dotspacemacs-install-packages 'used-but-keep-unused
   dotspacemacs-configuration-layers
   `(yxl-spacemacs

     ;; prog langs
     (ess :packages (not (org ess-smart-equals ess-R-object-popup))
          :variables
          ess-enable-smart-equals nil)

     emacs-lisp
     python
     vimscript
     yaml
     extra-langs
     windows-scripts
     autohotkey
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode)
     sql
     lua
     scala
     javascript
     html
     graphviz
     csv
     (shell :variables
            shell-default-shell (if (eq window-system 'w32) 'eshell
                                  'shell)
            ;; TODO: change back when upstream bug is fixed
            shell-default-height 60
            shell-default-position 'bottom
            shell-default-full-span t)

     ;; note takings
     (deft :variables
       deft-directory "~/Dropbox/org/notes")
     (org :packages (not evil-org org-projectile)
          :variables org-enable-reveal-js-support t)

     ;; markup langs
     ;; markdown: need vmd from npm: npm install -g vmd
     (markdown :packages (not mmm-mode)
               :variables markdown-live-preview-engine 'vmd)
     (pandoc :packages (not ox-pandoc))
     (latex :variables
            latex-build-command "LatexMk"
            latex-enable-auto-fill nil
            latex-enable-folding t)
     bibtex

     ;; editing
     (vinegar :packages (not dired))

     ;; major util modes
     (spell-checking :variables
                     spell-checking-enable-by-default nil)
     (syntax-checking :variables
                      syntax-checking-enable-by-default nil)
     ivy
     (auto-completion :variables
                      ;; use tab to complete
                      ;; return key enters new line
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-enable-sort-by-usage nil
                      ;; completion tool tip, nil, buggy in evil
                      auto-completion-enable-help-tooltip nil
                      :disabled-for     ; layer name
                      org
                      markdown)
     ,(unless (spacemacs/system-is-mswindows)
        'version-control)
     ,(unless (spacemacs/system-is-mswindows)
        '(git :variables
              git-magit-status-fullscreen t))
     ,(unless (spacemacs/system-is-mswindows)
        'github)

     ;; minor utils modes
     nlinum
     colors
     (ibuffer :variables
              ;; dont set to projects, really freaking slow
              ibuffer-group-buffers-by 'modes)
     search-engine
     better-defaults
     imenu-list

     ;; my private layers
     calfw
     ov-highlighter
     yxl-pdf-tools
     (yxl-utils)
     yxl-edit
     (yxl-clojure)
     yxl-evil
     yxl-prog
     yxl-text
     yxl-workspace
     yxl-config
     yxl-dired
     (yxl-datascience :variables
                      yxl-datascience-docset-path "~/Dropbox/dash-docsets"
                      :packages
                      (not ein))
     (yxl-org :variables
              yxl-org-babel-languages '((dot . t)
                                        (latex .t)
                                        (python .t)
                                        (sh .t)
                                        (R .t)
                                        (ipython . t)
                                        (scala . t)))
     yxl-web
     (yxl-completion)

     yxl-ui
     (yxl-email :packages
                (not mu4e-alert)
                :variables
                mu4e-installation-path "/usr/share/emacs/site-lisp/mu4e"
                yxl-email-personal-config-file (concat yxl-path-personal
                                                       "yxl-emacs-mu4e.el"))
     (yxl-elfeed :variables
                 elfeed-enable-web-interface t
                 yxl-elfeed-db-directory "~/Dropbox/rss/.elfeed"
                 yxl-elfeed-personal-config-file (concat yxl-path-personal
                                                         "yxl-elfeed-configs.el"))
     yxl-misc)))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   ;; dotspacemacs-startup-banner (concat dotspacemacs-directory
   ;;                                     "assets/emacs-china-icon-small.png")
   dotspacemacs-startup-banner 0
   dotspacemacs-startup-lists '((projects . 5)
                                (recents . 10))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'fundamental-mode
   dotspacemacs-themes '(yxl-gruv-dark
                         yxl-gruv-light
                         yxl-solar-dark
                         sanityinc-tomorrow-night)
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
   dotspacemacs-emacs-command-key ":"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab t
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
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
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title nil
   dotspacemacs-show-transient-state-color-guide nil
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-smooth-scrolling nil
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'origami
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'current
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'trailing))

(defun dotspacemacs/user-init ()
  (setq yxl-path-dotfiles "~/dotfiles/")
  (setq yxl-path-personal "~/dotfiles/personal/")
  (setq org-directory "~/Dropbox/org/")
  ;; load path
  ;; Project structure:
  ;; - config: ideally only configuration code, i.e. no defun
  ;; - lisp: self funcs and routines, global to the project
  ;; - site-lisp: 3rd-party original / self-modified modules
  ;; - theme: colorschemes and ui stuff
  (add-to-list 'load-path (concat dotspacemacs-directory "config"))
  (add-to-list 'load-path (concat dotspacemacs-directory "lisp"))
  (add-to-list 'load-path (concat dotspacemacs-directory "site-lisp"))
  (add-to-list 'load-path (concat dotspacemacs-directory "theme/yxl-theme"))
  (add-to-list 'load-path (concat dotspacemacs-directory "theme/yxl-airline"))
  (add-to-list 'load-path (concat dotspacemacs-directory "theme/yxl-airline-theme"))
  (add-to-list 'custom-theme-load-path
               (concat dotspacemacs-directory "theme/yxl-theme"))
  (add-to-list 'custom-theme-load-path
               (concat dotspacemacs-directory "theme/yxl-airline-theme"))
  ;; my env
  (load-file (concat dotspacemacs-directory "config/yxl-env.el"))
  (add-to-list 'load-path (concat yxl-path-dotfiles "yxl-emacs-goodies"))
  (add-to-list 'load-path (concat yxl-path-dotfiles "yxl-emacs-datascience-goodies"))
  ;; init stage config
  (load-file (concat dotspacemacs-directory "config/config-init.el"))
  (load-file (concat dotspacemacs-directory "config/hack.el"))
  (load-file (concat yxl-path-personal "yxl-personal.el"))
  ;; custom.el
  (setq-default custom-file (expand-file-name "custom.el" dotspacemacs-directory))
  (load custom-file 'no-error 'no-message))

(defun dotspacemacs/user-config ()
  (load-theme 'yxl-airline t)
  ;; add hook so that modeline colors are set correctly after theme change
  (add-hook 'spacemacs-post-theme-change-hook #'yxl-airline-theme-set-colors)
  (load-file (concat dotspacemacs-directory "config/config-post-init.el"))
  (load-file (concat dotspacemacs-directory "lisp/yxl-hydra.el"))
  (load-file (concat dotspacemacs-directory "lisp/general.el"))
  (load-file (concat dotspacemacs-directory "config/yxl-keybindings.el"))
  (load-file (concat dotspacemacs-directory "config/hack-post-init.el"))
  (load-file (concat dotspacemacs-directory "config/config-last.el")))
