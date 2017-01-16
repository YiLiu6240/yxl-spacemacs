;; mac: use super+3 as "#"
(global-set-key (kbd "s-3") (lambda () (interactive) (insert "#")))
;; (global-set-key (kbd "C-S-p") #'helm-M-x)
(global-set-key (kbd "C-S-p") #'counsel-M-x)
(global-set-key (kbd "C-S-o") #'company-yasnippet)
(global-set-key (kbd "C-S-y") #'yas-insert-snippet)
(global-set-key (kbd "C-h") #'delete-backward-char)
(define-key isearch-mode-map "\C-h" #'isearch-delete-char)

;; overwrite stock bindings
(spacemacs/set-leader-keys
  ";" #'counsel-M-x  ; prevent overwritten by nerd-commenter
  "<SPC>" #'evil-avy-goto-char-2
  "av" #'yxl-org/agenda-view
  "ak" #'calendar
  "aK" #'cfw-open-calendar
  "tow" #'yxl-web-switch-browser
  "bB" #'yxl-buffer-switch-same-major-mode
  ;; TODO: rm this with next spacemacs update
  "bm" #'view-echo-area-messages
  "fY" #'yxl-show-and-copy-buffer-filename-in-projectile
  "bY" #'yxl-buffer-store-name
  "bP" #'yxl-buffer-visit-stored-buffer
  "dd" #'yxl-ivy-switch-dired-buffer
  "i <SPC>" #'evil-insert-newline-around
  "ii" #'evil-insert-space
  "ia" #'evil-apend-space
  "hdF" #'counsel-faces
  "pG" #'projectile-regenerate-tags
  "tob" #'yxl-big-text-mode
  "tom" #'evil-show-marks
  "ws" #'split-window-below-and-focus
  "wS" #'split-window-below
  "wv" #'split-window-right-and-focus
  "wV" #'split-window-right
  "wY" #'yxl-window-record-layout
  "wP" #'yxl-window-load-layout
  "w <SPC>" #'ace-window
  "w M-h" #'buf-move-left
  "w M-j" #'buf-move-down
  "w M-k" #'buf-move-up
  "w M-l" #'buf-move-right
  "l" #'spacemacs/workspaces-transient-state/body
  "LY" #'yxl-workspace/record-config
  "LP" #'yxl-workspace/load-config)

;; leader keys
(spacemacs/set-leader-keys
  ".."  #'spacemacs/scale-font-transient-state/body
  "./"  #'spacemacs/zoom-frm-transient-state/body
  ".s"  #'yxl-append-to-scratch
  ".mm" #'bm-toggle
  ".mM" #'bm-toggle-buffer-persistence
  ".mk" #'bm-previous
  ".mj" #'bm-next
  ".mK" #'bm-previous
  ".mJ" #'bm-next
  ".ms" #'bm-show
  ".mS" #'bm-show-all
  ".ww" #'window-configuration-to-register
  ".wj" #'jump-to-register
  ".Ss1" #'yxl-session-save-1
  ".Sl1" #'yxl-session-load-1
  ".Ss2" #'yxl-session-save-2
  ".Sl2" #'yxl-session-load-2
  ".vp" #'ivy-push-view
  ".vP" #'ivy-pop-view

  ;; top-level quick actions
  "o-" #'yxl-dired-popup

  ;; cite
  "occ" #'helm-bibtex
  "ocg" #'gscholar-bibtex

  ;; dictionary
  "odd" #'helm-dictionary
  "odb" #'bing-dict-brief
  "ods" #'synonyms

  ;; frames
  "obb" #'ibuffer
  "obc" #'clone-indirect-buffer-other-window
  "obd" #'delete-other-windows
  "obn" #'yxl-buffer-inherit
  "o <SPC>" #'delete-other-windows
  "obr" #'revert-buffer

  ;; file
  "ofb" #'yxl-find-file-bib
  "off" #'yxl-find-file/body

  ;; Frame
  "oFM" #'make-frame
  "oFN" #'set-frame-name

  ;; ess
  "oe2" #'yxl-ess-repl-2cols
  "oe3" #'yxl-ess-repl-3cols

  ;; insert
  "ois" 'yas-insert-snippet

  ;; mode
  "omh" #'html-mode
  "oml" #'latex-mode
  "omm" #'gfm-mode
  "omo" #'org-mode
  "omp" #'python-mode
  "omr" #'R-mode
  "ome" #'emacs-lisp-mode

  "oo" #'yxl-hydra-hotspot/body

  ;; orgmode TODO: use hydra
  "oOo" #'org-agenda
  "oO1" #'yxl-org/agenda-work
  "oO0" #'yxl-org/agenda-life

  ;; project
  "op"  #'yxl-find-dir/body

  ;; quick
  "ogg" #'yxl-helm-hotspot
  "ogo" #'yxl-helm-org-files
  "ogf" #'yxl-helm-files
  "ogw" #'yxl-helm-websites

  ;; search/scratch
  "osg" #'helm-google-suggest

  "oy" #'copy-file-name-to-clipboard

  ;; window
  "ow1" #'yxl-window-custom-layout1
  "ow2" #'yxl-window-custom-layout2
  "ow3" #'yxl-window-vertical-3
  ;; "ows" #'split-window-below-small
  "ows" #'yxl-window-split-horizontal-focus
  "owS" #'yxl-window-split-horizontal-stay
  "owv" #'yxl-window-split-vertical-focus
  "owV" #'yxl-window-split-vertical-stay
  "owg" #'yxl-window-adjust-width-ratio
  "owh" #'yxl-window-adjust-height-ratio
  "owp" #'yxl-window-get-buffer-previous-window
  "owc" #'yxl-window-center-margins
  "oww" #'yxl-window-change-width

  ;; text
  "oxp" #'counsel-yank-pop
  "oxh" #'highlight-regexp)

;; prefixes
(spacemacs/declare-prefix "." "user-quick")
(spacemacs/declare-prefix ".S" "session")
(spacemacs/declare-prefix ".m" "bm")
(spacemacs/declare-prefix ".w" "window-register")
(spacemacs/declare-prefix "l" "layout")
(spacemacs/declare-prefix "L" "Layout")

(spacemacs/declare-prefix "o" "user-own")
(spacemacs/declare-prefix "ob" "buffer")
(spacemacs/declare-prefix "oc" "cite")
(spacemacs/declare-prefix "oC" "cheatsheet")
(spacemacs/declare-prefix "od" "dictionary")
(spacemacs/declare-prefix "oF" "Frames")
(spacemacs/declare-prefix "oe" "ess")
(spacemacs/declare-prefix "of" "quick-files")
(spacemacs/declare-prefix "oi" "insert")
(spacemacs/declare-prefix "om" "modes")
(spacemacs/declare-prefix "oo" "org-mode")
(spacemacs/declare-prefix "op" "quick-projects")
(spacemacs/declare-prefix "os" "search/scratch")
(spacemacs/declare-prefix "ox" "text")
(spacemacs/declare-prefix "ow" "window-layouts")
