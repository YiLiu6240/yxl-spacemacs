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
     calfw
     graphviz
     pdf-tools
     imenu-list

     ;; private --------
     helm-dash
     calfw
     yxl-layers)))

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
   dotspacemacs-scratch-mode 'gfm-mode
   dotspacemacs-themes '(yxl-dark
                         spacegray
                         sanityinc-tomorrow-night
                         gruvbox
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
   ;; dotspacemacs-folding-method 'evil
   dotspacemacs-folding-method 'origami
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

  (load-file (concat dotspacemacs-directory "config/yxl-init-env.el"))
  (load-file (concat dotspacemacs-directory "config/init-config.el"))
  (load-file (concat dotspacemacs-directory "config/hack.el"))
  (load-file (concat dotspacemacs-directory "config/yxl-spacemacs-home.el"))

  (setq-default custom-file (expand-file-name "custom.el" dotspacemacs-directory))
  (load custom-file 'no-error 'no-message)

  (if (eq system-type 'windows-nt)
      (progn
        ;; use external ls
        ;; use `where' as equivalent of `which'
        (setq ls-lisp-use-insert-directory-program t)
             (setq insert-directory-program "c:/Yi_Liu_Apps/Git/bin/ls")))
  (if (eq system-type 'darwin)
      ;; osx setup of pdf-tools
      ;; http://emacs.stackexchange.com/questions/13314/install-pdf-tools-on-emacs-macosx
      (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo"))
  )

(defun dotspacemacs/user-config ()
  ;; --------
  ;; general
  ;; --------
  (load-file (concat dotspacemacs-directory "config/yxl-bindings.el"))
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
  (add-to-list 'auto-mode-alist '("\\.sublime-settings$" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.sublime-keymap$" . json-mode))

  ;; misc stuff
  (setq-default require-final-newline t)
  (setq-default auto-revert-interval 60)
  ;; -----------
  ;; ==== general packages below ====
  ;; -----------
  (setq hl-todo-keyword-faces '(("HOLD" . "#268bd2")
                                ("TODAY" . "#dc322f")
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
  (add-to-list 'display-buffer-alist '("\\*PDF" display-buffer-at-bottom))


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

  (add-hook 'lua-mode-hook (lambda () (setq-local origami-fold-style 'triple-braces)))
  (add-hook 'bibtex-mode-hook (lambda () (setq-local origami-fold-style 'triple-braces)))
  (setq origami-show-fold-header t)
  ;; --------
  ;; misc configs
  ;; --------
  (setq bookmark-default-file "~/Dropbox/inbox/helm-bookmark")

  (setq lua-indent-level 4)

  ;; tramp bug, from zilongshanren
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
  )
