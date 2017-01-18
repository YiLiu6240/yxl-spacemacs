;; mac: use super+3 as "#"
(global-set-key (kbd "s-3") (lambda () (interactive) (insert "#")))
;; (global-set-key (kbd "C-S-p") #'helm-M-x)
(global-set-key (kbd "C-S-p") #'counsel-M-x)
(global-set-key (kbd "C-S-o") #'company-yasnippet)
(global-set-key (kbd "C-S-y") #'yas-insert-snippet)
(global-set-key (kbd "C-h") #'delete-backward-char)
(define-key isearch-mode-map "\C-h" #'isearch-delete-char)



(defhydra yxl-find-dir (:color blue)
  "Directory: "
  ("d" (find-file yxl-path-dotfiles) "dotfiles")
  ("g" (find-file yxl-path-downloads) "downloads")
  ("G" (find-file yxl-path-local) "local-repo")
  ("h" (find-file yxl-path-sync) "dropbox")
  ("H" (find-file yxl-path-projects) "projects")
  ("o" (find-file yxl-path-org) "org")
  ("c" (find-file yxl-path-code-pwd) "code")
  ("p" (find-file yxl-path-paper-pwd) "papers")
  ("j" (find-file yxl-path-journal-pwd) "journals")
  ("b" (find-file yxl-path-book-reference) "books"))

(defhydra yxl-find-file (:color blue)
  "File: "
  ("1" (yxl-find-file-stay yxl-file-org-main) "tasks_1_main.org")
  ("2" (yxl-find-file-stay yxl-file-org-work) "tasks_2_work.org")
  ("3" (yxl-find-file-stay yxl-file-org-config) "tasks_3_config.org")
  ("4" (yxl-find-file-stay yxl-file-org-proj) "tasks_4_proj.org")
  ("0" (yxl-find-file-stay yxl-file-org-scratch) "scratch.org")
  ("b" (yxl-find-file-stay yxl-file-bib) "bib file")
  ("n" (yxl-find-file-stay yxl-file-note-master) "note file")
  ("e" (yxl-find-file-stay "~/Dropbox/Inbox/scratch.el") "scratch.el"))

(defhydra yxl-hydra-hotspot (:color blue :hint nil)
  "
Hotspot:
 | _h_ Frame: Meta | _0_ Org: scratch | _k_: calendar     |
 | _j_ Frame: REPL | _1_ Org: main    | _K_: cfw-calendar |
 | _k_ Frame: Code | _2_ Org: work    | _o_: org-files    |
 | _l_ Frame: Conf | _3_ Org: config  | _a_: org-agenda   |
 | ^^              | _4_ Org: proj    | _v_: org-agenda_view |
"
  ("h" (yxl-frame-select-or-set "Meta"))
  ("j" (yxl-frame-select-or-set "REPL"))
  ("k" (yxl-frame-select-or-set "Code"))
  ("l" (yxl-frame-select-or-set "Conf"))
  ("o" (yxl-org-open-all-task-files) "org: open all tasks")
  ("a" (org-agenda-list) "org: agenda")
  ("k" calendar)
  ("K" cfw-open-calendar)
  ("v" yxl-org/agenda-view)
  ("1" (yxl-find-file-popup yxl-file-org-main))
  ("2" (yxl-find-file-popup yxl-file-org-work))
  ("3" (yxl-find-file-popup yxl-file-org-config))
  ("4" (yxl-find-file-popup yxl-file-org-proj))
  ("0" (yxl-find-file-popup yxl-file-org-scratch)))

(defhydra yxl-hydra-system (:color blue :hint nil)
  "
System:
 | _f_: +font-size      ^^^^          | _F_: +Frame-size  | _T_: +Transparency |
 | _b_: big text        ^^^^          | _B_: invlidate bg | _m_: evil marks    | _M_: menubar |
 | _w_: switch browser  ^^^^          |
 | _t_/_d_/_D_: timer:start/stop/down |
"
  ("," #'eval-expression "M-:")
  ("f" #'spacemacs/scale-font-transient-state/body)
  ("F" #'spacemacs/zoom-frm-transient-state/body)
  ("T" #'spacemacs/scale-transparency-transient-state/spacemacs/toggle-transparency)
  ("b" yxl-big-text-mode)
  ("B" (set-face-background 'default "unspecified-bg" (selected-frame)))
  ("w" yxl-web-switch-browser)
  ("m" evil-show-marks)
  ("M" menu-bar-mode)
  ("t" mode-line-timer-start)
  ("d" mode-line-timer-stop)
  ("D" mode-line-timer-done))



;; overwrite stock bindings
(spacemacs/set-leader-keys
  ";" #'counsel-M-x  ; prevent overwritten by nerd-commenter
  "<SPC>" #'evil-avy-goto-char-2
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
  "LP" #'yxl-workspace/load-config
  "xh" #'yxl-ov-highlighter/body
  "xH" #'highlight-regexp)



(spacemacs/set-leader-keys
  "." #'yxl-hydra-system/body)

(spacemacs/declare-prefix "o" "user-own")
(spacemacs/set-leader-keys
  "o-" #'yxl-dired-popup
  "o <SPC>" #'delete-other-windows
  "of" #'yxl-find-file/body
  "oy" #'copy-file-name-to-clipboard
  "oo" #'yxl-hydra-hotspot/body
  "op"  #'yxl-find-dir/body)

(spacemacs/declare-prefix "ob" "buffer")
(spacemacs/set-leader-keys
  "obb" #'ibuffer
  "obc" #'clone-indirect-buffer-other-window
  "obd" #'delete-other-windows
  "obn" #'yxl-buffer-inherit
  "obr" #'revert-buffer)

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

(spacemacs/declare-prefix "oe" "ess")
(spacemacs/set-leader-keys
  "oe2" #'yxl-ess-repl-2cols
  "oe3" #'yxl-ess-repl-3cols)

(spacemacs/declare-prefix "og" "Helm-hotspot")
(spacemacs/set-leader-keys
  "ogg" #'yxl-helm-hotspot
  "ogo" #'yxl-helm-org-files
  "ogf" #'yxl-helm-files
  "ogw" #'yxl-helm-websites)

(spacemacs/declare-prefix "oi" "insert")
(spacemacs/set-leader-keys
  "ois" #'yas-insert-snippet
  "oia" #'yxl-append-to-scratch)

(spacemacs/declare-prefix "om" "modes")
(spacemacs/set-leader-keys
  "omh" #'html-mode
  "oml" #'latex-mode
  "omm" #'gfm-mode
  "omo" #'org-mode
  "omp" #'python-mode
  "omr" #'R-mode
  "ome" #'emacs-lisp-mode)

(spacemacs/declare-prefix "os" "search")
(spacemacs/set-leader-keys
  "osg" #'helm-google-suggest)

(spacemacs/declare-prefix "oS" "session")
(spacemacs/set-leader-keys
  "oSs1" #'yxl-session-save-1
  "oSl1" #'yxl-session-load-1
  "oSs2" #'yxl-session-save-2
  "oSl2" #'yxl-session-load-2)

(spacemacs/declare-prefix "ox" "text")
(spacemacs/set-leader-keys
  "oxp" #'counsel-yank-pop)

(spacemacs/declare-prefix "ow" "window")
(spacemacs/set-leader-keys
  "ow1" #'yxl-window-custom-layout1
  "ow2" #'yxl-window-custom-layout2
  "ow3" #'yxl-window-vertical-3
  "ows" #'yxl-window-split-horizontal-focus
  "owS" #'yxl-window-split-horizontal-stay
  "owv" #'yxl-window-split-vertical-focus
  "owV" #'yxl-window-split-vertical-stay
  "owg" #'yxl-window-adjust-width-ratio
  "owh" #'yxl-window-adjust-height-ratio
  "owp" #'yxl-window-get-buffer-previous-window
  "owc" #'yxl-window-center-margins
  "oww" #'yxl-window-change-width)
