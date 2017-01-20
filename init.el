;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs-base
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-additional-packages '()
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '(persp-mode
                                    smooth-scrolling
                                    spaceline
                                    window-numbering
                                    org-bullets
                                    tern)
   dotspacemacs-install-packages 'used-only
   dotspacemacs-configuration-layers
   '(yxl-spacemacs
     better-defaults

     ;; major langs --------
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode)
     emacs-lisp
     (ess :packages (not org))
     git
     lua
     ;; markdown: need vmd from npm: npm install -g vmd
     (markdown :variables markdown-live-preview-engine 'vmd)
     (org :packages (not evil-org))
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
                      :disabled-for  ; layer name
                      org
                      markdown)
     ;; semantic
     pandoc
     (shell :variables
            shell-default-shell (if (eq window-system 'w32) 'eshell
                                  'shell)
            shell-default-height 30
            shell-default-position 'bottom
            shell-default-full-span nil)
     (spell-checking :variables
                     spell-checking-enable-by-default nil)
     (syntax-checking :variables
                      syntax-checking-enable-by-default nil)
     ;; vim/evil mode --------
     (vinegar :packages (not dired))
     ;; evil-cleverparens

     ;; other layers --------
     ;; smex
     colors
     (ibuffer :variables
              ;; dont set to projects, really freaking slow
              ibuffer-group-buffers-by 'modes)
     search-engine
     deft
     (elfeed :variables
             rmh-elfeed-org-files (list "~/dotfiles/rss/feeds.org"))
     github

     ;; os-dependent --------
     ;; osx

     ;; tools --------
     csv
     graphviz
     pdf-tools
     imenu-list

     ;; my private layers --------
     calfw
     ov-highlighter
     yxl-utils
     yxl-workspace
     yxl-config
     yxl-evil
     yxl-prog
     yxl-text
     yxl-dired
     (yxl-dash :variables
               yxl-dash-docset-newpath "~/Dropbox/dash-docsets"
               yxl-dash-browser-func 'w3m-goto-url-new-session)
     yxl-ess
     yxl-org
     yxl-web
     yxl-completion
     yxl-ui
     yxl-misc)))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists nil
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'markdown-mode
   dotspacemacs-themes '(yxl-gruv
                         yxl-dark
                         spacegray
                         sanityinc-tomorrow-night
                         gruvbox
                         spacemacs-dark
                         solarized-dark
                         monokai
                         spacemacs-light
                         solarized-light)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font `(,(cond ((eq system-type 'windows-nt)
                                       "InputMonoNarrow")
                                      ((eq system-type 'darwin)
                                       "InputMonoNarrow")
                                      (t "Input Mono Narrow"))
                               :size ,(cond ((eq system-type 'darwin) 12)
                                            ((eq system-type 'windows-nt) 12)
                                            (t 12))
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
   dotspacemacs-folding-method 'origami
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup `all))

(defun dotspacemacs/user-init ()
  (add-to-list 'load-path "~/.spacemacs.d/config")
  (add-to-list 'load-path "~/.spacemacs.d/site-lisp")
  (add-to-list 'load-path "~/.spacemacs.d/theme/yxl-theme")
  (add-to-list 'load-path "~/dotfiles/yxl-emacs-goodies")

  (add-to-list 'custom-theme-load-path "~/.spacemacs.d/theme/yxl-theme")

  (load-file (concat dotspacemacs-directory "config/config-init.el"))
  (load-file (concat dotspacemacs-directory "config/yxl-env.el"))
  (load-file (concat dotspacemacs-directory "config/hack.el"))
  (load-file (concat dotspacemacs-directory "config/yxl-spacemacs-home.el"))
  (load-file "~/dotfiles/personal/yxl-personal.el")

  (setq-default custom-file (expand-file-name "custom.el" dotspacemacs-directory))
  (load custom-file 'no-error 'no-message))

(defun dotspacemacs/user-config ()
  (load-file (concat dotspacemacs-directory "config/config-post-init.el"))
  (load-file (concat dotspacemacs-directory "config/yxl-bindings.el"))
  (load-file (concat dotspacemacs-directory "config/hack-post-init.el"))
  (load-file (concat dotspacemacs-directory "config/config-last.el")))
