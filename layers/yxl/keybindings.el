(spacemacs/set-leader-keys
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

  ;; top-level quick actions
  "o1" #'select-frameGroup-Code
  "o2" #'select-frameGroup-REPL
  "o3" #'select-frameGroup-Meta
  "o4" #'select-frameGroup-Config
  "ok" #'select-frameGroup-Code
  "oj" #'select-frameGroup-REPL
  "oh" #'select-frameGroup-Meta
  "ol" #'select-frameGroup-Config

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
  "obn" #'spacemacs/new-empty-buffer
  "obr" #'revert-buffer

  ;; file
  "ofb" #'yxl/find-file-bib
  "ofo" #'yxl/find-file-org
  "ofO" #'yxl/find-file-org-work
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
  "oFs1" #'set-frameGroup-Code
  "oFs2" #'set-frameGroup-REPL
  "oFs3" #'set-frameGroup-Meta
  "oFs4" #'set-frameGroup-Config

  "oFsh" #'set-frameGroup-Meta
  "oFsj" #'set-frameGroup-REPL
  "oFsk" #'set-frameGroup-Code
  "oFsl" #'set-frameGroup-Config

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
