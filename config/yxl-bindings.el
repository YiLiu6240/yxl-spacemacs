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
  "oak" #'calendar
  "oac" #'cfw-open-calendar
  "fY" #'yxl-show-and-copy-buffer-filename-in-projectile
  "bY" #'yxl-buffer-store-name
  "bP" #'yxl-buffer-visit-stored-buffer
  "i <SPC>" #'evil-insert-newline-around
  "ii" #'evil-insert-space
  "ia" #'evil-apend-space
  "pG" #'projectile-regenerate-tags
  "tob" #'yxl-big-text-mode
  "tom" #'evil-show-marks
  "ws" #'split-window-below-and-focus
  "wS" #'split-window-below
  "wv" #'split-window-right-and-focus
  "wV" #'split-window-right
  "wY" #'yxl-window-record-layout
  "wP" #'yxl-window-load-laytout
  "w <SPC>" #'ace-window
  "WY" #'yxl-workspace/record-config
  "WP" #'yxl-workspace/load-config)

;; addition to stock bindings
(spacemacs/set-leader-keys
  "hdF" #'counsel-faces
  "dd" #'ivy-switch-dired-buffer)

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
  "o1" #'yxl-find-file-org-popup
  "o2" #'yxl-find-file-org-work-popup
  "o3" #'yxl-find-file-org-config-popup
  "o4" #'yxl-find-file-org-proj-popup
  "o0" #'yxl-find-file-org-scratch-popup
  "o-" #'dired-popup
  "ok" #'yxl-frame-select-code
  "oj" #'yxl-frame-select-repl
  "oh" #'yxl-frame-select-meta
  "ol" #'yxl-frame-select-config

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
  "ofn" #'yxl-find-file-note
  "ofN" #'yxl-find-file-note-master
  "ofm" #'yxl-text/find-TeX-master
  "ofr" #'yxl-text/find-project-root
  "ofu" #'yxl-text/find-project-outline

  ;; Frame
  "oFM" #'make-frame
  "oFN" #'set-frame-name
  "oFn" #'yxl-frame-select-by-name
  "oFs1" #'yxl-frame-set-code
  "oFs2" #'yxl-frame-set-repl
  "oFs3" #'yxl-frame-set-meta
  "oFs4" #'yxl-frame-set-config

  "oFsh" #'yxl-frame-set-meta
  "oFsj" #'yxl-frame-set-repl
  "oFsk" #'yxl-frame-set-code
  "oFsl" #'yxl-frame-set-config

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

  ;; orgmode
  "ooo" #'yxl-helm-org-files
  "oo1" #'yxl-org/agenda-work
  "oo0" #'yxl-org/agenda-life

  ;; project
  "opg" #'yxl-find-dir-downloads
  "opG" #'yxl-find-dir-local
  "oph" #'yxl-find-dir-dropbox
  "opc" #'yxl-find-pwd-code
  "opd" #'yxl-find-dir-dotfiles
  "opj" #'yxl-find-pwd-journal
  "opo" #'yxl-find-dir-org
  "opp" #'yxl-find-pwd-paper

  ;; quick
  "og" #'yxl-helm-quick

  ;; search
  "osg" #'helm-google-suggest

  "oy" #'copy-file-name-to-clipboard

  ;; window
  "ow1" #'yxl-window-custom-layout1
  "ow2" #'yxl-window-custom-layout2
  "ow3" #'yxl-window-custom-layout3
  ;; "ows" #'split-window-below-small
  "ows" #'yxl-window-split-horizontal-focus
  "owS" #'yxl-window-split-horizontal-stay
  "owv" #'yxl-window-split-vertical-focus
  "owV" #'yxl-window-split-vertical-stay
  "owg" #'yxl-window-adjust-width-ratio
  "owh" #'yxl-window-adjust-height-ratio
  "owp" #'yxl-window-get-buffer-previous-window
  "owc" #'yxl-window-center-margins
  "oww" #'yxl-window-change-width)

;; prefixes
(spacemacs/declare-prefix "." "user-quick")
(spacemacs/declare-prefix ".S" "session")
(spacemacs/declare-prefix ".m" "bm")
(spacemacs/declare-prefix ".w" "window-register")

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
(spacemacs/declare-prefix "ow" "window-layouts")
