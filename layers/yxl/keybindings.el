(spacemacs/set-leader-keys
  ".."  #'yxl/append-to-scratch
  ".s"  #'scratch-pop
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
  ".Ss" #'session-save
  ".Sl" #'session-load
  ".vp" #'ivy-push-view
  ".vP" #'ivy-pop-view
  ".po" #'yxl/find-file-org-popup
  ".pw" #'yxl/find-file-org-work-popup

  ;; top-level quick actions
  "o1" #'select-frame-code
  "o2" #'select-frame-REPL
  "o3" #'select-frame-meta
  "o4" #'select-frame-config
  "ok" #'select-frame-code
  "oj" #'select-frame-REPL
  "oh" #'select-frame-meta
  "ol" #'select-frame-config

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
  "o <SPC>" #'delete-other-windows
  "obr" #'revert-buffer

  ;; file
  "ofb" #'yxl/find-file-bib
  "ofo" #'yxl/find-file-org-other-window
  "ofO" #'yxl/find-file-org
  "ofw" #'yxl/find-file-org-work-other-window
  "ofW" #'yxl/find-file-org-work
  "ofd" #'yxl/find-file-diary
  "ofs" #'spacemacs/find-dotfile
  "ofn" #'yxl/find-file-note
  "ofN" #'yxl/find-file-note-master
  "ofm" #'yxl-text/find-TeX-master
  "ofr" #'yxl-text/find-project-root
  "ofu" #'yxl-text/find-project-outline

  ;; Frame
  "oFM" #'make-frame
  "oFN" #'set-frame-name
  "oFn" #'select-frame-by-name
  "oFs1" #'set-frame-code
  "oFs2" #'set-frame-REPL
  "oFs3" #'set-frame-meta
  "oFs4" #'set-frame-config

  "oFsh" #'set-frame-meta
  "oFsj" #'set-frame-REPL
  "oFsk" #'set-frame-code
  "oFsl" #'set-frame-config

  ;; ess
  "oe2" #'yxl/ess-repl-2cols
  "oe3" #'yxl/ess-repl-3cols

  ;; insert
  "ois" 'yas-insert-snippet

  ;; mode
  "omh" #'html-mode
  "oml" #'latex-mode
  "omm" #'gfm-mode
  "omo" #'org-mode
  "omp" #'python-mode
  "omr" #'R-mode

  ;; orgmode
  "ooo" #'org-agenda
  "ooa" #'yxl/helm-find-org-agenda
  "oo1" #'yxl/org-agenda-work
  "oo0" #'yxl/org-agenda-life

  ;; project
  "opg" #'yxl/find-dir-Downloads
  "oph" #'yxl/find-dir-Dropbox
  "opc" #'yxl/find-pwd-code
  "opd" #'yxl/find-dir-dotfiles
  "opj" #'yxl/find-pwd-journal
  "opo" #'yxl/find-dir-org
  "opp" #'yxl/find-pwd-paper

  ;; quick
  "og" #'yxl/helm-quick

  ;; search
  "osg" #'helm-google-suggest

  ;; visual/view
  "ovm" #'evil-show-marks
  "ovt" #'yxl/view-today-sidebar
  "ovg" #'yxl/view-todo-panel

  "oy" #'copy-file-name-to-clipboard

  ;; window
  "ow1" #'yxl/custom-layout-1
  "ow2" #'yxl/custom-layout-2
  "ow3" #'yxl/custom-layout-3
  "ows" #'split-window-below-small
  "owS" #'split-window-above-small
  "owv" #'split-window-right-small
  "owV" #'split-window-left-small
  "owc" #'yxl/center-window-margins
  "oww" #'yxl/change-window-width)

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
