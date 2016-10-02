;; -*- mode: emacs-lisp -*-

;; (package-initialize)
(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '()
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
     (auto-completion :variables
                      ;; use tab to complete
                      ;; return key enters new line
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-enable-sort-by-usage t
                      ;; completion tool tip, nil, buggy in evil
                      auto-completion-enable-help-tooltip nil)
     eyebrowse
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
     unimpaired
     vinegar
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
   dotspacemacs-additional-packages '(ereader)
   dotspacemacs-excluded-packages '(org-repo-todo
                                    org-bullets
                                    ido
                                    ;; persp-mode
                                    vi-tilde-fringe)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '(recents bookmarks projects)
   dotspacemacs-startup-recent-list-size 5
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(yxl-dark
                         spacemacs-dark
                         solarized-dark
                         monokai
                         spacemacs-light
                         solarized-light)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font `("Input Mono Narrow"
                               ;; :size 12
                               :size ,(if (eq system-type 'darwin) 12 12)
                               ;; :size ,(if (spacemacs/system-is-mswindows) 16 13)
                               :weight normal
                               :width normal
                               :powerline-scale 1.0)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-command-key ":"
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-use-ido nil
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-micro-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling nil
   dotspacemacs-line-numbers nil
   dotspacemacs-smartparens-strict-mode nil
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

  (load-file (concat dotspacemacs-directory "config/hack.el"))
  (load-file (concat dotspacemacs-directory "config/init-config.el"))

)

(defun dotspacemacs/user-config ()
  ;; --------
  ;; general
  ;; --------
  (load-file (concat dotspacemacs-directory "config/yxl-keys.el"))
  ;; emacs general
   ;; osx
  (setq-default mac-option-modifier 'super
                mac-command-modifier 'meta)
  (setq-default ns-use-srgb-colorspace nil)

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

  (with-eval-after-load 'desktop-save-mode
   (add-to-list 'desktop-modes-not-to-save 'image-mode)
   (add-to-list 'desktop-modes-not-to-save 'elfeed-search-mode)
   (add-to-list 'desktop-modes-not-to-save 'elfeed-show-mode)
   (add-to-list 'desktop-modes-not-to-save 'pdf-view-mode))

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
  (setq custom-file (expand-file-name "custom.el" dotspacemacs-directory))
  (load custom-file)

  ;; ---- end of dotspacemacs/user-config ----
  )
