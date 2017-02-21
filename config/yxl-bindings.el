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


(defhydra yxl-find-dir-hydra (:color blue :hint nil :columns 4)
  "
Directory:
"
  ("d" (yxl-find-dir yxl-path-dotfiles) "dotfiles")
  ("D" (yxl-find-dir "~/.emacs.d") ".emacs.d")
  ("g" (yxl-find-dir yxl-path-downloads) "downloads")
  ("G" (yxl-find-dir yxl-path-local) "local-repo")
  ("h" (yxl-find-dir yxl-path-sync) "dropbox")
  ("H" (yxl-find-dir yxl-path-projects) "projects")
  ("o" (yxl-find-dir yxl-path-org) "org")
  ("c" (yxl-find-dir yxl-path-code-pwd) "code")
  ("p" (yxl-find-dir yxl-path-paper-pwd) "papers")
  ("j" (yxl-find-dir yxl-path-journal-pwd) "journals")
  ("b" (yxl-find-dir yxl-path-book-reference) "books"))

(defhydra yxl-find-file-hydra (:color blue :columns 4
                                      :pre (setq which-key-inhibit t)
                                      :post (setq which-key-inhibit nil))
  "
File:
"
  ("1" (yxl-find-file-stay yxl-file-org-main) "tasks_1_main.org")
  ("2" (yxl-find-file-stay yxl-file-org-work) "tasks_2_work.org")
  ("3" (yxl-find-file-stay yxl-file-org-config) "tasks_3_config.org")
  ("4" (yxl-find-file-stay yxl-file-org-proj) "tasks_4_proj.org")
  ("0" (yxl-find-file-stay yxl-file-org-scratch) "scratch.org")
  ("b" (yxl-find-file-stay yxl-file-bib) "bib file")
  ("n" (yxl-find-file-stay yxl-file-note-master) "note file")
  ("e" (yxl-find-file-stay "~/Dropbox/inbox/scratch.el") "scratch.el")
  ("sf" (yxl-find-file-stay yxl-file-sites-local) "sites: local")
  ("sw" (yxl-find-file-stay yxl-file-sites-web) "sites: web")
  ("rf" (yxl-find-file-stay yxl-file-reading-list-files) "reading-list: files")
  ("rw" (yxl-find-file-stay yxl-file-reading-list-webpages) "reading-list: webpages"))

(defhydra yxl-hydra-common (:color blue :hint nil)
  ("." nil "quit")
  ("q" nil "quit")
  ("+" make-frame "make-frame")
  ("s" yxl-hydra-sessions/body "sessions"))

(defhydra yxl-hydra-hotspot (:color blue :hint nil
                                    :pre (setq which-key-inhibit t)
                                    :post (setq which-key-inhibit nil)
                                    :inherit (yxl-hydra-common/heads))
  "

Hotspot:

 | [_h_]: Frame: Meta             | [_0_]: Org: scratch                 |
 | [_j_]: Frame: REPL             | [_1_]: Org: main                    |
 | [_k_]: Frame: Code             | [_2_]: Org: work                    |
 | [_l_]: Frame: Conf             | [_3_]: Org: config                  |
 | ^^                             | [_4_]: Org: proj                    |

 | [_cK_]: calendar               | [_gg_]: Helm: my hotspot            |
 | [_ck_]: cfw-calendar           | ^^                                  |
 | [_cc_]: Org: capture           | [_go_]: Helm: my org files          |
 | [_oa_]: Org: agenda list       | [_gs_]: Helm: my local/web shortcuts|
 | [_ov_]: Org: calendar/agenda   | ^^                                  |
 | [_ot_]: Org: todo list         | [_gr_]: Helm: my reading list       |
 | [_oo_]: Org: open task files   | ^^                                  |
 | [_oO_]: Org: open all files    | ^^                                  |

 | [_ia_]: append: to *scratch*   | ^^                                  |
 | [_is_]: append: to scratch.org | ^^                                  |
"

  ("h" (yxl-frame-select-or-set "Meta"))
  ("j" (yxl-frame-select-or-set "REPL"))
  ("k" (yxl-frame-select-or-set "Code"))
  ("l" (yxl-frame-select-or-set "Conf"))

  ("0" (yxl-find-file-popup yxl-file-org-scratch))
  ("1" (yxl-find-file-popup yxl-file-org-main))
  ("2" (yxl-find-file-popup yxl-file-org-work))
  ("3" (yxl-find-file-popup yxl-file-org-config))
  ("4" (yxl-find-file-popup yxl-file-org-proj))
  ("5" (yxl-find-file-popup yxl-file-org-local))

  ("cK" calendar)
  ("ck" cfw-open-calendar)

  ("cc" org-capture)
  ("oo" (lambda ()
          (interactive)
          (delete-other-windows)
          ;; open main org files
          (yxl-find-file-open-all `(,yxl-file-org-main
                                    ,yxl-file-org-work
                                    ,yxl-file-org-config
                                    ,yxl-file-org-proj))
          ;; open scratch as sidebar
          (yxl-find-file-popup yxl-file-org-scratch)
          (split-window-below-and-focus)
          (find-file yxl-file-org-local)
          ;; adjust height
          (evil-window-right 1)
          (let ((current-prefix-arg 80))
            (yxl-window-adjust-height-ratio))))
  ("oO" yxl-org-open-all-task-files)
  ("oa" org-agenda-list)
  ("ov" yxl-org/agenda-view)
  ("ot" org-todo-list)

  ("gg" yxl-helm-hotspot)
  ("go" yxl-helm-org-files)
  ("gs" yxl-helm-shortcuts)
  ("gr" yxl-helm-reading-list)

  ("is" (yxl-append-to-scratch yxl-file-org-scratch))
  ("ia" yxl-append-to-scratch))

(defhydra yxl-hydra-system (:color blue :hint nil
                                   :pre (setq which-key-inhibit t)
                                   :post (setq which-key-inhibit nil)
                                   :inherit (yxl-hydra-common/heads))
  "
location: %(if buffer-file-name buffer-file-name default-directory)
major-mode: %`major-mode
pos: %(line-number-at-pos)/%(line-number-at-pos (point-max)):%(current-column)
evil-shift-width: %`evil-shift-width; tabs: %`indent-tabs-mode
encoding: %`buffer-file-coding-system

--------
 [_Su_]: update Spacemacs ^^^^ [_SU_]: update packages [_SR_]: roll back packages

 [_f_]: +font-size ^^^^        [_F_]: +Frame-size      [_T_]: +Transparency

 [_tw_]: switch browser  ^^^^  [_tb_]: big text

 [_att_/_atd_/_atD_]: timer:start/stop/down
"
  ("," #'eval-expression "M-:")

  ("Su" (lambda ()
          (interactive)
          (spacemacs/home)
          (spacemacs/switch-to-version)))
  ("SU" (lambda ()
          (interactive)
          (spacemacs/home)
          (configuration-layer/update-packages)))
  ("SR" (lambda ()
          (interactive)
          (spacemacs/home)
          (call-interactively 'configuration-layer/rollback)))

  ("f" #'spacemacs/scale-font-transient-state/body)
  ("F" #'spacemacs/zoom-frm-transient-state/body)
  ("T" #'spacemacs/scale-transparency-transient-state/spacemacs/toggle-transparency)

  ("tw" yxl-web-switch-browser)
  ("tm" menu-bar-mode)
  ("tb" yxl-big-text-mode)

  ("att" mode-line-timer-start)
  ("atd" mode-line-timer-stop)
  ("atD" mode-line-timer-done))

(defhydra yxl-hydra-sessions (:color blue :hint nil :columns 4
                                     :pre (setq which-key-inhibit t)
                                     :post (setq which-key-inhibit nil))
  ("s" yxl-ivy-views-switch "ivy-views: switch")
  ("vs" yxl-ivy-views-save "ivy-views: save")
  ("vl" yxl-ivy-views-load "ivy-views: load")
  ("vp" yxl-ivy-push-view "ivy-views: push")
  ("vP" ivy-pop-view "ivy-views: pop")
  ("vK" (setq ivy-views nil) "ivy-views: clean")
  ("ve" (find-file yxl-ivy-views-storage-location) "ivy-views: edit")
  ("Ss1" yxl-session-save-1 "desktop: profile1, save")
  ("Sl1" yxl-session-load-1 "desktop: profile1, load")
  ("Ss2" yxl-session-save-2 "desktop: profile2, save")
  ("Sl2" yxl-session-load-2 "desktop: profile2, load")
  ("b" helm-bookmarks "helm: bookmarks"))



;; overwrite stock bindings
(spacemacs/set-leader-keys
  "<SPC>" #'evil-avy-goto-char-2
  "bB" #'yxl-buffer-switch-same-major-mode
  "bh" #'yxl-utils/home
  ;; TODO: rm this with next spacemacs update
  "bm" #'view-echo-area-messages
  "ff" #'yxl-find-file-counsel
  "fY" #'yxl-show-and-copy-buffer-filename-in-projectile
  "fo" #'spacemacs/open-file-or-directory-in-external-app ; with C-u open in desktop
  "fO" #'yxl-open-file-external
  "fp" #'counsel-projectile-find-file
  "fP" #'find-file-in-project-truename
  "bY" #'yxl-buffer-store-name
  "bP" #'yxl-buffer-visit-stored-buffer
  "dd" #'yxl-dired-ivy-switch-buffer
  "i <SPC>" #'evil-insert-newline-around
  "ii" #'evil-insert-space
  "ia" #'evil-apend-space
  "hdF" #'counsel-faces
  "l" #'spacemacs/workspaces-transient-state/body
  "LY" #'yxl-workspace/record-config
  "LP" #'yxl-workspace/load-config
  "pm" #'helm-make
  "pM" #'helm-make-projectile
  "pG" #'projectile-regenerate-tags
  "p C-g" nil
  "qf" #'spacemacs/frame-killer
  "sJ" #'yxl-imenu-anywhere
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
  "xh" #'highlight-regexp
  "xH" #'yxl-ov-highlighter/body
  "xa{" 'spacemacs/align-repeat-left-curly-brace
  "xa}" 'spacemacs/align-repeat-right-curly-brace
  "xa[" 'spacemacs/align-repeat-left-square-brace
  "xa]" 'spacemacs/align-repeat-right-square-brace)



(spacemacs/set-leader-keys
  "." #'yxl-hydra-system/body)

(spacemacs/declare-prefix "o" "user-own")
(spacemacs/set-leader-keys
  ";" #'counsel-M-x
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
  "occ" #'org-ref-helm-insert-cite-link
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

(spacemacs/declare-prefix "ox" "text")
(spacemacs/set-leader-keys
  "oxp" #'counsel-yank-pop)
