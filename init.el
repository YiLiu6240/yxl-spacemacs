;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs-base
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(yxl-spacemacs
     better-defaults

     ;; major langs --------
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode)
     emacs-lisp
     (ess :packages (not org))
     (git :variables
          git-magit-status-fullscreen nil)
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
                      :disabled-for     ; layer name
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
     ;; (evil-snipe :variables
     ;;             ;; f/t not bound to current line
     ;;             evil-snipe-enable-alternate-f-and-t-behaviors t)
     evil-cleverparens

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

     ;; tools --------
     csv
     calfw
     graphviz
     pdf-tools
     imenu-list

     ;; private --------
     helm-dash

     yxl-layers)
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(persp-mode
                                    smooth-scrolling
                                    spaceline
                                    window-numbering
                                    org-bullets
                                    tern
                                    ido)
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
   dotspacemacs-emacs-command-key ";"
   dotspacemacs-ex-command-key ";"
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
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup `all))

(defun dotspacemacs/user-init ()
  (add-to-list 'load-path "~/.spacemacs.d/config/")
  (add-to-list 'load-path "~/.spacemacs.d/lisp/")
  (add-to-list 'load-path "~/.spacemacs.d/theme/yxl-theme/")

  (add-to-list 'custom-theme-load-path "~/.spacemacs.d/theme/yxl-theme/")

  (load-file (concat dotspacemacs-directory "config/yxl-init-def.el"))
  (load-file (concat dotspacemacs-directory "config/init-config.el"))
  (load-file (concat dotspacemacs-directory "config/hack.el"))
  (load-file (concat dotspacemacs-directory "config/yxl-spacemacs-home.el"))

  (setq-default custom-file (expand-file-name "custom.el" dotspacemacs-directory))
  (load custom-file 'no-error 'no-message))

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
  (setq-default initial-major-mode 'gfm-mode)
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
  (add-to-list 'auto-mode-alist '("\\.inbox$" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.tex$" . latex-mode))

  ;; misc stuff
  (setq-default require-final-newline t)
  (setq-default auto-revert-interval 60)
  ;; -----------
  ;; ==== general packages below ====
  ;; -----------
  (setq hl-todo-keyword-faces '(("HOLD" . "#268bd2")
                                ("TODO" . "#cb4b16")
                                ("NEXT" . "#859900")
                                ("THEM" . "#859900")
                                ("PROG" . "#cb4b16")
                                ("ISSUE" . "#cb4b16")
                                ("DOING" . "#b58900")
                                ("WIP" . "#b58900")
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
  ;; display-buffer-alist
  ;; --------
  ;; shell: display in current window
  (add-to-list 'display-buffer-alist '("\\`\\*e?shell" display-buffer-same-window))
  (add-to-list 'display-buffer-alist '("\\*R" display-buffer-same-window))
  ;; FIXME: not working
  (add-to-list 'display-buffer-alist '("\\*Python\\*" display-buffer-same-window))
  (add-to-list 'display-buffer-alist '("\\*magit:" display-buffer-same-window))

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
  (add-hook 'spacemacs-buffer-mode-hook
            (lambda () (define-key spacemacs-buffer-mode-map
                         "o" 'widget-button-press)))

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
  ;; under REVIEW
  ;; --------
  (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
  (evil-define-key 'normal evil-cleverparens-mode-map (kbd "H") nil)
  (evil-define-key 'normal evil-cleverparens-mode-map (kbd "L") nil)
  ;; evil-cp overwrites y, cannot use reguster 0
  (evil-define-key 'normal evil-cleverparens-mode-map (kbd "y") nil)
  (evil-define-key 'visual evil-cleverparens-mode-map (kbd "y") nil)
  ;; dont overwrite evil unimpared
  (evil-define-key 'normal evil-cleverparens-mode-map (kbd "[") nil)
  (evil-define-key 'visual evil-cleverparens-mode-map (kbd "[") nil)
  (evil-define-key 'normal evil-cleverparens-mode-map (kbd "]") nil)
  (evil-define-key 'visual evil-cleverparens-mode-map (kbd "]") nil)

  ;; --------
  ;; misc configs
  ;; --------
  (setq bookmark-default-file "~/Dropbox/inbox/helm-bookmark")

  (setq lua-indent-level 4)

  ;; tramp bug, from zilongshanren
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

  ;; final steps
  (switch-to-buffer "*scratch*"))
