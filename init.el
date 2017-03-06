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
                                    tern
                                    ess-smart-equals)
   dotspacemacs-install-packages 'used-only
   dotspacemacs-configuration-layers
   '(yxl-spacemacs

     ;; prog langs
     (ess :packages (not (org ess-smart-equals))
          :variables
          ess-enable-smart-equals nil)

     emacs-lisp
     python
     ipython-notebook
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

     ;; markup langs
     (org :packages (not evil-org)
          :variables org-enable-reveal-js-support t)
     (markdown :variables markdown-live-preview-engine 'vmd)
     ;; markdown: need vmd from npm: npm install -g vmd
     pandoc
     (latex :variables
            latex-build-command "LatexMk"
            latex-enable-auto-fill nil
            latex-enable-folding t)
     bibtex

     ;; editing
     (vinegar :packages (not dired))
     (evil-snipe :variables
                 evil-snipe-enable-alternate-f-and-t-behaviors t)

     ;; major util modes
     (elfeed :packages (not elfeed-org))
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
                      auto-completion-enable-sort-by-usage t
                      ;; completion tool tip, nil, buggy in evil
                      auto-completion-enable-help-tooltip nil
                      :disabled-for  ; layer name
                      org
                      markdown)
     git
     github

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
     yxl-utils
     yxl-edit
     yxl-evil
     yxl-prog
     yxl-text
     yxl-workspace
     yxl-config
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
   dotspacemacs-scratch-mode 'fundamental-mode
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
                                       "hack")
                                      ((eq system-type 'darwin)
                                       "hack")
                                      (t "hack"))
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
   dotspacemacs-mode-line-unicode-symbols nil
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
  ;; load path
  (add-to-list 'load-path (concat dotspacemacs-directory "config"))
  (add-to-list 'load-path (concat dotspacemacs-directory "site-lisp"))
  (add-to-list 'load-path (concat dotspacemacs-directory "theme/yxl-theme"))
  (add-to-list 'load-path (concat dotspacemacs-directory "theme/yxl-airline"))
  (add-to-list 'load-path (concat dotspacemacs-directory "theme/yxl-airline-theme"))
  (add-to-list 'custom-theme-load-path (concat dotspacemacs-directory "theme/yxl-theme"))
  (add-to-list 'custom-theme-load-path (concat dotspacemacs-directory "theme/yxl-airline-theme"))
  ;; my env
  (load-file (concat dotspacemacs-directory "config/yxl-env.el"))
  (add-to-list 'load-path (concat yxl-path-dotfiles "yxl-emacs-goodies"))
  ;; init stage config
  (load-file (concat dotspacemacs-directory "config/config-init.el"))
  (load-file (concat dotspacemacs-directory "config/hack.el"))
  (load-file (concat dotspacemacs-directory "config/yxl-spacemacs-home.el"))
  (load-file "~/dotfiles/personal/yxl-personal.el")
  ;; custom.el
  (setq-default custom-file (expand-file-name "custom.el" dotspacemacs-directory))
  (load custom-file 'no-error 'no-message))

(defun dotspacemacs/user-config ()
  (load-theme 'yxl-airline t)
  (load-file (concat dotspacemacs-directory "config/config-post-init.el"))
  (load-file (concat dotspacemacs-directory "config/yxl-bindings.el"))
  (load-file (concat dotspacemacs-directory "config/yxl-hydra.el"))
  (load-file (concat dotspacemacs-directory "config/hack-post-init.el"))
  (load-file (concat dotspacemacs-directory "config/config-last.el")))
