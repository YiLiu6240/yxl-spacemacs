(defhydra yxl-hydra-common (:color blue :hint nil)
  ("." nil "quit")
  ("q" nil "quit"))

(defhydra yxl-hydra-access (:color blue :hint nil
                                   :inherit (yxl-hydra-common/heads))
  ("+" make-frame "make-frame")
  ("s" yxl-hydra-sessions/body "sessions"))

(defhydra yxl-hydra-window (:color blue :hint nil
                                   :inherit (yxl-hydra-common/heads))
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


(defhydra yxl-hydra-find-dir (:color blue :hint nil :columns 4
                                     :inherit (yxl-hydra-common/heads))
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

(defhydra yxl-hydra-find-file (:color blue :columns 4
                                      :pre (setq which-key-inhibit t)
                                      :post (setq which-key-inhibit nil)
                                      :inherit (yxl-hydra-common/heads))
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

(defhydra yxl-hydra-hotspot (:color blue :hint nil
                                    :pre (setq which-key-inhibit t)
                                    :post (setq which-key-inhibit nil)
                                    :inherit (yxl-hydra-access/heads))
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
                                   :inherit (yxl-hydra-access/heads))
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
                                     :post (setq which-key-inhibit nil)
                                     :inherit (yxl-hydra-common/heads))
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
