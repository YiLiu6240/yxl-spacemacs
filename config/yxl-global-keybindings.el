;; mac: use super+3 as "#"
(global-set-key (kbd "s-3") (lambda () (interactive) (insert "#")))
(global-set-key (kbd "C-S-p") #'counsel-M-x)
(global-set-key (kbd "C-h") #'delete-backward-char)
(global-set-key (kbd "M-q") #'spacemacs/toggle-truncate-lines)

(global-set-key (kbd "C-s") #'save-buffer)

(global-set-key (kbd "M-<SPC>") #'yas-or-company)
(define-key isearch-mode-map "\C-h" #'isearch-delete-char)

;; Mouse wheel zooming
;; wheel down
(global-set-key (kbd "<C-mouse-5>") #'spacemacs/scale-down-font)
;; wheel up
(global-set-key (kbd "<C-mouse-4>") #'spacemacs/scale-up-font)

;; overwrite stock bindings
(spacemacs/set-leader-keys
  "," #'yxl-hydra-hotspot/body
  "." #'yxl-hydra-system/body
  "'" #'yxl-shell-invoke
  "<SPC>" #'yxl-hydra-space/body
  "aa" #'yxl-invoke-applications
  "ac" #'cfw/open-calendar
  "aC" #'calc-dispatch
  "bB" #'yxl-buffer-switch-same-major-mode
  ;; "bb" #'yxl-buffer-switch
  "bb" #'ivy-switch-buffer
  "bh" #'yxl-base/home
  "bH" #'yxl-spacemacs-dashboard
  "bob" #'ibuffer
  "bon" #'yxl-buffer-inherit
  "bc" #'yxl-buffer-compilation
  "bC" #'clone-indirect-buffer-other-window
  "bi" #'imenu-list-smart-toggle
  "bm" #'yxl-buffer-switch-same-major-mode
  "bM" #'spacemacs/switch-to-messages-buffer
  "br" #'revert-buffer
  "bY" #'yxl-buffer-store-name
  "bP" #'yxl-buffer-visit-stored-buffer
  "bx" #'kill-buffer-and-window
  "dd" #'yxl-dired-ivy-switch-buffer
  "fed" #'spacemacs/find-dotfile-follow-symlink
  "ff" #'yxl-find-file-counsel
  "fh" #'helm-find-files-vertical
  "fJ" #'dired-jump-split
  "fY" #'yxl-show-and-copy-buffer-filename-in-projectile
  "fO" #'spacemacs/open-file-or-directory-in-external-app ; with C-u open in desktop
  "fo" #'yxl-open-file-external
  "fp" #'counsel-projectile-find-file ; respects .projectile subprojects
  "fP" #'counsel-git                  ; treats the git repo as the whole porject
  "f C-p" #'find-file-in-project-truename ; does not respect gitignore, thus can find ignored files
  "g S" #'magit-status-fullscreen
  "g C-s" #'magit-stage-file
  "hdF" #'counsel-faces
  "i <SPC>" #'yxl-evil-insert-newline-around
  "ii" #'yxl-evil-insert-space
  "ia" #'yxl-evil-append-space
  "iI" #'yxl-evil-insert-space-around
  "is" #'yas-insert-snippet
  "i <tab>" #'company-complete
  "jj" #'evil-avy-goto-char-2
  "l" #'spacemacs/workspaces-transient-state/body
  "LY" #'yxl-workspace/record-config
  "LP" #'yxl-workspace/load-config
  "nr" #'ni-narrow-to-region-indirect-same-window
  "ph" #'yxl-project-helm
  "ps" #'yxl-project-shell-popup
  "p'" nil
  "pc" #'yxl-project-cite
  "pC" #'projectile-compile-project
  "pG" #'projectile-regenerate-tags
  "p C-g" nil
  "pO" #'yxl-project-select
  "po" #'yxl-project-popup
  "pm" #'helm-make-projectile
  "pM" #'helm-make
  "pt" #'treemacs-projectile-toggle
  "qf" #'spacemacs/frame-killer
  "sj" #'counsel-semantic-or-imenu
  "sJ" #'helm-semantic-or-imenu
  "s C-j" #'yxl-imenu-anywhere
  "tv" #'yxl-hydra-visual-line/body
  "tb" #'yxl-web-switch-browser
  "tp" nil
  "tpp" #'parinfer-toggle-mode
  "tP" #'spacemacs/toggle-smartparens
  "w0" #'delete-other-windows
  "w1" #'delete-other-windows
  "ws" #'split-window-below-and-focus
  "wS" #'split-window-below
  "wv" #'split-window-right-and-focus
  "wV" #'split-window-right
  "wY" #'yxl-window-record-layout
  "wP" #'yxl-window-load-layout
  "w M-h" #'buf-move-left
  "w M-j" #'buf-move-down
  "w M-k" #'buf-move-up
  "w M-l" #'buf-move-right
  "wx" #'kill-buffer-and-window
  "xh" #'highlight-regexp
  "xH" #'yxl-ov-highlighter/body
  "xa{" 'spacemacs/align-repeat-left-curly-brace
  "xa}" 'spacemacs/align-repeat-right-curly-brace
  "xa[" 'spacemacs/align-repeat-left-square-brace
  "xa]" 'spacemacs/align-repeat-right-square-brace)

(spacemacs/declare-prefix "o" "user-own")
(spacemacs/set-leader-keys
  ";" #'counsel-M-x
  "o-" #'yxl-dired-popup
  "o <SPC>" #'delete-other-windows
  "of" #'yxl-hydra-files
  "og" #'yxl-helm-hotspot
  "oy" #'copy-file-name-to-clipboard
  "oo" #'yxl-hydra-hotspot/body
  "op" #'yxl-hydra-projects
  "ot" #'ivy-todo
  "ow" #'yxl-hydra-window/body)

(spacemacs/declare-prefix "oc" "cite")
(spacemacs/set-leader-keys
  "occ" #'helm-bibtex
  "ocg" #'gscholar-bibtex)

(spacemacs/declare-prefix "od" "dictionary")
(spacemacs/set-leader-keys
  "odd" #'helm-dictionary
  "odb" #'bing-dict-brief
  "ods" #'synonyms)

(spacemacs/declare-prefix "oF" "Frames")
(spacemacs/set-leader-keys
  "oFM" #'make-frame
  "oFN" #'set-frame-name)

(spacemacs/declare-prefix "oi" "insert")
(spacemacs/set-leader-keys
  "ois" #'yas-insert-snippet)

(spacemacs/declare-prefix "om" "modes")
(spacemacs/set-leader-keys
  "omh" #'html-mode
  "oml" #'latex-mode
  "omm" #'markdown-mode
  "omM" #'gfm-mode
  "omo" #'org-mode
  "omp" #'python-mode
  "omr" #'R-mode
  "ome" #'emacs-lisp-mode)

(spacemacs/declare-prefix "os" "search")
(spacemacs/set-leader-keys
  "osg" #'helm-google-suggest)

(spacemacs/declare-prefix "ox" "text")
(spacemacs/set-leader-keys
  "oxp" #'counsel-yank-pop)
