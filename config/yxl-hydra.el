;; hydra colors:
;; - red: continue
;; - blue: stop

(defhydra yxl-hydra-common (:color blue :hint nil)
  ("." nil "quit")
  ("q" nil "quit"))

(defhydra yxl-hydra-access (:color blue :hint nil
                                   :inherit (yxl-hydra-common/heads))
  ("+" make-frame "make-frame")
  ("-" yxl-dired-popup "dired-popup")
  ("s" yxl-hydra-sessions/body "sessions"))

(defhydra yxl-hydra-window (:color blue :hint nil
                                   :pre (setq which-key-inhibit t)
                                   :post (setq which-key-inhibit nil)
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

  ("l1" yxl-window-custom-layout1 "layout: 1")
  ("l2" yxl-window-custom-layout2 "layout: 2")
  ("lv2" yxl-window-layout-v2h2 "layout: v2h2")
  ("lh3" yxl-window-layout-h3 "layout: h3")
  ("lv3" yxl-window-layout-v3h2 "layout: v3h2")

  ("wp" yxl-window-get-buffer-previous-window "previous buffer")
  ("wc" yxl-window-center-margins "center margin")
  ("ww" yxl-window-change-width "adjust width"))

(defhydra yxl-hydra-ace-window (:color blue :hint nil :columns 4
                                       :inherit (yxl-hydra-common/heads))
  ("s" ace-swap-window "ace: swap window")
  ("M" ace-maximize-window "ace: max window")
  ("<SPC>" ace-select-window "ace: select window")
  ("d" ace-delete-window "ace: delete window")
  ("p" yxl-ace-window-push-window "ace: push window")
  ("f" yxl-ace-window-fetch-window "ace: fetch window"))

(defhydra yxl-hydra-find-dir (:color blue :hint nil :columns 4
                                     :inherit (yxl-hydra-common/heads))
  "
Directory:
"
  ("d" (yxl-find-dir "~/Downloads/Desktop") "desktop")
  ("D" (yxl-find-dir yxl-path-dotfiles) "dotfiles")
  ("e" (yxl-find-dir "~/.emacs.d") ".emacs.d")
  ("g" (yxl-find-dir yxl-path-downloads) "downloads")
  ("G" (yxl-find-dir yxl-path-local) "local-repo")
  ("h" (yxl-find-dir yxl-path-sync) "dropbox")
  ("H" (yxl-find-dir yxl-path-projects) "projects")
  ("o" (find-file yxl-path-org) "org")
  ("c" (yxl-find-dir yxl-path-code-pwd) "code")
  ("C" (yxl-find-dir yxl-path-code-master-pwd) "code-master")
  ("p" (yxl-find-dir yxl-path-paper-pwd) "papers")
  ("j" (yxl-find-dir yxl-path-journal-pwd) "journals")
  ("J" (yxl-find-dir yxl-path-journal-more-pwd) "Journals")
  ("b" (yxl-find-dir yxl-path-book-reference) "books")
  ("s" (yxl-find-dir (concat yxl-path-local
                             "yxl_datascience")) "datascience"))

(defhydra yxl-hydra-find-file (:color blue :columns 4
                                      :pre (setq which-key-inhibit t)
                                      :post (setq which-key-inhibit nil)
                                      :inherit (yxl-hydra-common/heads))
  "
File:
"
  ("1" (yxl-find-file-stay yxl-file-org-todo) "todo.org")
  ("0" (yxl-find-file-stay yxl-file-org-quick) "quick.org")
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

 | [_h_]: Frame: h                | [_0_]: Org: scratch                 |
 | [_j_]: Frame: j                | [_1_]: Org: todo                    |
 | [_k_]: Frame: k                | ^^                                  |
 | [_l_]: Frame: l                | ^^                                  |

 | [_cK_]: calendar               | [_gg_]: Helm: my hotspot            |
 | [_ck_]: cfw-calendar           | ^^                                  |
 | [_cc_]: Org: capture           | [_go_]: Helm: my org files          |
 | [_oa_]: Org: agenda: life      | [_gs_]: Helm: my local/web shortcuts|
 | [_oA_]: Org: agenda: work      | ^^                                  |
 | [_ol_]: Org: log               | [_gr_]: Helm: my reading list       |
 | [_oo_]: Org: project view      | ^^                                  |
 | [_oO_]: Org: open all files    | ^^                                  |

 | [_ia_]: append: to *scratch*   | ^^                                  |
 | [_is_]: append: to quick.org   | ^^                                  |
"

  ("h" (yxl-frame-select-or-set "Frame-h"))
  ("j" (yxl-frame-select-or-set "Frame-j"))
  ("k" (yxl-frame-select-or-set "Frame-k"))
  ("l" (yxl-frame-select-or-set "Frame-l"))

  ;; org files opening logic
  ;; - 0 - 5: open them in "sidebar"
  ;; - C-u / Meta + 0 - 5: open them in current window
  ;; - Shift + 0 - 5: open them in bottom popup
  ("0" (yxl-find-file-popup yxl-file-org-quick))
  ("M-0" (find-file yxl-file-org-quick))
  ("1" (yxl-find-file-popup yxl-file-org-todo))
  ("M-1" (find-file yxl-file-org-todo))
  (")" (popwin:popup-buffer (find-file-noselect yxl-file-org-quick)
                            :stick t
                            :height 0.4
                            :position 'bottom))
  ("!" (popwin:popup-buffer (find-file-noselect yxl-file-org-todo)
                            :stick t
                            :height 0.4
                            :position 'bottom))
  ("cK" calendar)
  ("ck" cfw-open-calendar)

  ("cc" org-capture)
  ("oo" yxl-env-project-view)
  ("oO" yxl-org-open-all-task-files)
  ("oa" my-org-agenda-life)
  ("oA" my-org-agenda-work)
  ("ol" my-org-log)

  ("gg" yxl-helm-hotspot)
  ("go" yxl-helm-org-files)
  ("gs" yxl-helm-shortcuts)
  ("gr" yxl-helm-reading-list)

  ("is" (yxl-append-to-scratch yxl-file-org-quick))
  ("ia" yxl-append-to-scratch))

(defhydra yxl-hydra-system (:color blue :hint nil
                                   :pre (setq which-key-inhibit t)
                                   :post (setq which-key-inhibit nil)
                                   :inherit (yxl-hydra-access/heads))
  "
# File:
- location: %(if buffer-file-name buffer-file-name default-directory)
- major-mode: %`major-mode
- pos: %(line-number-at-pos)/%(line-number-at-pos (point-max)):%(current-column)
- evil-shift-width: %`evil-shift-width; indent-tabs-mode: %`indent-tabs-mode
- encoding: %`buffer-file-coding-system

# Project:
- git-branch: %(if (featurep 'magit) (magit-get-current-branch) \"magit not started\")

# Buffer & Frame:
- Font-zoomed-scale: %(when (featurep 'face-remap) text-scale-mode-amount)
- Frame-zoomed-scale: %(frame-parameter nil 'zoomed)

# Emacs:
- browser-func: %`browse-url-browser-function

--------
 [_Su_]: update Spacemacs ^^^^ [_SU_]: update packages [_SR_]: roll back packages

 [_f_]: +font-size ^^^^        [_F_]: +Frame-size      [_T_]: +Transparency

 [_tw_]: switch browser  ^^^^  [_tb_]: big text

 [_att_/_atd_/_atD_]: timer:start/stop/down
"

  ("," #'set-frame-name "set-frame-name")

  ("<RET>" #'make-frame "make-frame")
  ("d" #'delete-frame  "delete-frame")

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
  ("F" #'yxl-hydra-frame-size/body)
  ("T" #'spacemacs/scale-transparency-transient-state/spacemacs/toggle-transparency)

  ("tw" yxl-web-switch-browser)
  ("tm" menu-bar-mode)
  ("tb" yxl-big-text-mode)

  ("att" mode-line-timer-start)
  ("atd" mode-line-timer-stop)
  ("atD" mode-line-timer-done))

(defhydra yxl-hydra-frame-size (:color red :hint nil)
  ("+" yxl-utils/frame-zoom-in "zoom-in")
  ("=" yxl-utils/frame-zoom-in "zoom-in")
  ("-" yxl-utils/frame-zoom-out "zoom-out")
  ("0" spacemacs/zoom-frm-unzoom "reset")
  ("1" (yxl-utils/frame-zoom-state 4) "profile 1")
  ("2" (yxl-utils/frame-zoom-state 6) "profile 2")
  ("3" (yxl-utils/frame-zoom-state 8) "profile 3")
  ("4" (yxl-utils/frame-zoom-state 12) "profile 4"))

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
