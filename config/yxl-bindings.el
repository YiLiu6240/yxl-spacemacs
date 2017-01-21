;; mac: use super+3 as "#"
(global-set-key (kbd "s-3") (lambda () (interactive) (insert "#")))
;; (global-set-key (kbd "C-S-p") #'helm-M-x)
(global-set-key (kbd "C-S-p") #'counsel-M-x)
(global-set-key (kbd "C-S-o") #'company-yasnippet)
(global-set-key (kbd "C-S-y") #'yas-insert-snippet)
(global-set-key (kbd "C-h") #'delete-backward-char)
(define-key isearch-mode-map "\C-h" #'isearch-delete-char)



(defhydra yxl-window-hydra (:color blue :hint nil)
  "
Window Split:

+------+----+  +---+---+---+  +-----------+  +-----------+
| S V  |    |  |C C|   | C |  |           |  |     |     |
+------+  v |  || || ->| | |  |---- g ----|  |     h     |
| s    |    |  |u u|   | u |  |           |  |     |     |
+------+----+  +---+---+---+  +-----------+  +-----------+

[_s_/_S_] sp: focus/stay [_v_/_V_] vsp: focus/stay

[_g_]: adjust width  [_h_]: adjust height
"
  ("q" nil "quit")
  ("." nil "quit")

  ("s" yxl-window-split-horizontal-focus)
  ("S" yxl-window-split-horizontal-stay)
  ("v" yxl-window-split-vertical-focus)
  ("V" yxl-window-split-vertical-stay)
  ("g" yxl-window-adjust-width-ratio)
  ("h" yxl-window-adjust-height-ratio)

  ("w1" yxl-window-custom-layout1 "layout: 1")
  ("w2" yxl-window-custom-layout2 "layout: 2")
  ("w3" yxl-window-vertical-3 "layout: v3")

  ("wp" yxl-window-get-buffer-previous-window "previous buffer")
  ("wc" yxl-window-center-margins "center margin")
  ("ww" yxl-window-change-width "adjust width"))


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

(defhydra yxl-find-file (:color blue
(defhydra yxl-find-dir-hydra (:color blue :hint nil)
(defhydra yxl-find-file-hydra (:color blue
                         :pre (setq which-key-inhibit t)
                         :post (setq which-key-inhibit nil))
  "File: "
  ("1" (yxl-find-file-stay yxl-file-org-main) "tasks_1_main.org")
  ("2" (yxl-find-file-stay yxl-file-org-work) "tasks_2_work.org")
  ("3" (yxl-find-file-stay yxl-file-org-config) "tasks_3_config.org")
  ("4" (yxl-find-file-stay yxl-file-org-proj) "tasks_4_proj.org")
  ("0" (yxl-find-file-stay yxl-file-org-scratch) "scratch.org")
  ("b" (yxl-find-file-stay yxl-file-bib) "bib file")
  ("n" (yxl-find-file-stay yxl-file-note-master) "note file")
  ("e" (yxl-find-file-stay "~/Dropbox/inbox/scratch.el") "scratch.el")
  ("rf" (yxl-find-file-stay yxl-file-reading-list-files) "reading-list: files")
  ("rw" (yxl-find-file-stay yxl-file-reading-list-webpages) "reading-list: webpages"))

(defhydra yxl-hydra-hotspot (:color blue :hint nil
                             :pre (setq which-key-inhibit t)
                             :post (setq which-key-inhibit nil))
  "

Hotspot:

 | [_h_]: Frame: Meta             | [_0_]: Org: scratch           |
 | [_j_]: Frame: REPL             | [_1_]: Org: main              |
 | [_k_]: Frame: Code             | [_2_]: Org: work              |
 | [_l_]: Frame: Conf             | [_3_]: Org: config            |
 | ^^                             | [_4_]: Org: proj              |

 | [_ck_]: calendar               | [_gg_]: Helm: my hotspot      |
 | [_cC_]: cfw-calendar           | [_go_]: Helm: my org files    |
 | [_cc_]: Org: capture           | [_gf_]: Helm: my files/dirs   |
 | [_oa_]: Org: agenda list       | [_gw_]: Helm: my websitess    |
 | [_ov_]: Org: calendar/agenda   | [_gr_]: Helm: my reading list |
 | [_ot_]: Org: todo list         | ^^                            |
 | [_oo_]: Org: open all files    |                               |

 | [_ia_]: append: to *scratch*   |                               |
 | [_is_]: append: to scratch.org |                               |
"
  ("q" nil "quit")
  ("." nil "quit")

  ("h" (yxl-frame-select-or-set "Meta"))
  ("j" (yxl-frame-select-or-set "REPL"))
  ("k" (yxl-frame-select-or-set "Code"))
  ("l" (yxl-frame-select-or-set "Conf"))

  ("0" (yxl-find-file-popup yxl-file-org-scratch))
  ("1" (yxl-find-file-popup yxl-file-org-main))
  ("2" (yxl-find-file-popup yxl-file-org-work))
  ("3" (yxl-find-file-popup yxl-file-org-config))
  ("4" (yxl-find-file-popup yxl-file-org-proj))

  ("ck" calendar)
  ("cC" cfw-open-calendar)

  ("cc" org-capture)
  ("oo" yxl-org-open-all-task-files)
  ("oa" org-agenda-list)
  ("ov" yxl-org/agenda-view)
  ("ot" org-todo-list)

  ("gg" yxl-helm-hotspot)
  ("go" yxl-helm-org-files)
  ("gf" yxl-helm-files)
  ("gw" yxl-helm-websites)
  ("gr" yxl-helm-reading-list)

  ("is" (yxl-append-to-scratch yxl-file-org-scratch))
  ("ia" yxl-append-to-scratch))

(defhydra yxl-hydra-system (:color blue :hint nil
                            :pre (setq which-key-inhibit t)
                            :post (setq which-key-inhibit nil))
  "

System:

 [_su_]: update Spacemacs [_sU_]: update packages [_sR_]: roll back packages

 | [_f_]: +font-size      ^^^^          | [_F_]: +Frame-size  | [_T_]: +Transparency |
 | [_b_]: big text        ^^^^          | [_B_]: invlidate bg | [_m_]: evil marks    |
 | [_w_]: switch browser  ^^^^          | [_M_]: menubar      |
 | [_t_/_d_/_D_]: timer:start/stop/down |
"
  ("q" nil "quit")
  ("." nil "quit")

  ("su" spacemacs/switch-to-version)
  ("sU" configuration-layer/update-packages)
  ("sR" (call-interactively 'configuration-layer/rollback))

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
  ("D" mode-line-timer-done)
  ("+" make-frame "make frame"))



;; overwrite stock bindings
(spacemacs/set-leader-keys
  ";" #'counsel-M-x  ; prevent overwritten by nerd-commenter
  "<SPC>" #'evil-avy-goto-char-2
  "bB" #'yxl-buffer-switch-same-major-mode
  "bh" #'yxl-utils/home
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
  "of" #'yxl-find-file-hydra/body
  "oy" #'copy-file-name-to-clipboard
  "oo" #'yxl-hydra-hotspot/body
  "op" #'yxl-find-dir-hydra/body
  "ow" #'yxl-window-hydra/body)

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

(spacemacs/declare-prefix "oi" "insert")
(spacemacs/set-leader-keys
  "ois" #'yas-insert-snippet)

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
