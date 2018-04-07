;; hydra colors:
;; - red: continue
;; - blue: stop

(defhydra yxl-hydra-common (:color blue :hint nil)
  ("." nil "quit")
  ("q" nil "quit"))

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

(defhydra yxl-hydra-space (:color blue :hint nil :columns 4
                                  :inherit (yxl-hydra-common/heads)
                                  :pre (setq which-key-inhibit t)
                                  :post (setq which-key-inhibit nil))
  "

ace-window:

 | [_ww_]: select   | [_ws_]: swap        | [_wM_]: max       |
 | [_wd_]: delete   | [_wp_]: push        | [_wf_]: fetch     |

---------------------------------------------------------------

avy:

 | [_jb_]: pop-mark | [_jj_]: goto-char-2 | [_jl_]: goto-line |
 | [_ju_]: goto-url | [_jw_]: goto-word   | [_xo_]: open-url  |
"
  ("ws" ace-swap-window)
  ("wM" ace-maximize-window)
  ("ww" ace-select-window)
  ("wd" ace-delete-window)
  ("wp" yxl-ace-window-push-window)
  ("wf" yxl-ace-window-fetch-window)
  ("jb" avy-pop-mark)
  ("jj" evil-avy-goto-char-2)
  ("jl" evil-avy-goto-line)
  ("ju" spacemacs/avy-goto-url)
  ("jw" evil-avy-goto-word-or-subword-1)
  ("xo" spacemacs/avy-open-url))

(defhydra yxl-hydra-hotspot (:color blue :hint nil
                                    :pre (setq which-key-inhibit t)
                                    :post (setq which-key-inhibit nil))
  "

Hotspot:

 | [_h_]: Frame: h                  | [_0_]: Org: scratch                 |
 | [_j_]: Frame: j                  | [_1_]: Org: todo                    |
 | [_k_]: Frame: k                  | ^^                                  |
 | [_l_]: Frame: l                  | ^^                                  |

 | [_cK_]: calendar                 | [_gg_]: Helm: my hotspot            |
 | [_ck_]: cfw-calendar             | ^^                                  |
 | [_cc_]: Org: capture             | [_go_]: Helm: my org files          |
 | [_oa_]: Org: agenda: life        | [_gs_]: Helm: my local/web shortcuts|
 | [_oA_]: Org: agenda: work        | ^^                                  |
 | [_ol_]: Org: log                 | [_gr_]: Helm: my reading list       |
 | [_oO_]: Org: open all files      | ^^                                  |

 | [_ia_]: append: to *scratch*     | ^^                                  |
 | [_is_]: append: to checkbox.org  | ^^                                  |
"

  ("h" (yxl-frame-select-or-set "Frame-h"))
  ("j" (yxl-frame-select-or-set "Frame-j"))
  ("k" (yxl-frame-select-or-set "Frame-k"))
  ("l" (yxl-frame-select-or-set "Frame-l"))

  ;; org files opening logic
  ;; - 0 - 5: open them in "sidebar"
  ;; - C-u / Meta + 0 - 5: open them in current window
  ;; - Shift + 0 - 5: open them in bottom popup
  ("0" (yxl-find-file-popup yxl-base-org-today))
  ("M-0" (find-file yxl-base-org-today))
  ("1" (yxl-find-file-popup yxl-base-org-todo))
  ("M-1" (find-file yxl-base-org-todo))
  (")" (popwin:popup-buffer (find-file-noselect yxl-base-org-today)
                            :stick t
                            :height 0.4
                            :position 'bottom))
  ("!" (popwin:popup-buffer (find-file-noselect yxl-base-org-todo)
                            :stick t
                            :height 0.4
                            :position 'bottom))
  ("cK" calendar)
  ("ck" cfw-open-calendar)

  ("cc" org-capture)
  ("oO" yxl-org-open-all-task-files)
  ("oa" my-org-agenda-life)
  ("oA" my-org-agenda-work)
  ("ol" my-org-log)

  ("gg" yxl-helm-hotspot)
  ("go" yxl-helm-org-files)
  ("gs" yxl-helm-shortcuts)
  ("gr" yxl-helm-reading-list)

  ("is" (yxl-append-to-scratch yxl-base-org-today))
  ("ia" yxl-append-to-scratch))

(defhydra yxl-hydra-system (:color blue :hint nil
                                   :pre (setq which-key-inhibit t)
                                   :post (setq which-key-inhibit nil))
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

  ("." make-frame "make-frame")
  ("," #'set-frame-name "set-frame-name")

  ("<RET>" #'make-frame "make-frame")
  ("d" #'delete-frame  "delete-frame")

  ("-" yxl-dired-popup "dired-popup")
  ("s" yxl-hydra-sessions/body "sessions")
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
  ("+" yxl-ui/frame-zoom-in "zoom-in")
  ("=" yxl-ui/frame-zoom-in "zoom-in")
  ("-" yxl-ui/frame-zoom-out "zoom-out")
  ("0" spacemacs/zoom-frm-unzoom "reset")
  ("1" (yxl-ui/frame-zoom-state 4) "profile 1")
  ("2" (yxl-ui/frame-zoom-state 6) "profile 2")
  ("3" (yxl-ui/frame-zoom-state 8) "profile 3")
  ("4" (yxl-ui/frame-zoom-state 12) "profile 4"))

(defhydra yxl-hydra-visual-line (:color amaranth :hint nil
                                        :pre (setq which-key-inhibit t)
                                        :post (setq which-key-inhibit nil))
  "
visual-line:
------------
[_vv_]: ?vv? visual-line-mode
[_vf_]: ?vf? visual-fill-column-mode
[_vc_]: ?vc? visual-fill-column-center-tex
[_vF_]: ?vF? focus-mode
[_v._]: visual-fill-column-width: ?v.?
------------
"
  ("vv" visual-line-mode
   (if visual-line-mode "[x]" "[ ]"))
  ("vf" (if visual-fill-column-mode
            (progn (visual-fill-column-mode -1))
          (progn
            (visual-fill-column-mode 1)
            (visual-line-mode 1)))
   (if (bound-and-true-p visual-fill-column-mode) "[x]" "[ ]"))
  ("vc" (progn (setq visual-fill-column-center-text
                     (not visual-fill-column-center-text))
               (visual-fill-column-mode 1) (visual-line-mode 1))
   (if (bound-and-true-p visual-fill-column-center-text) "[x]" "[ ]"))
  ("vF" (if focus-mode
            (focus-mode -1)
          (focus-mode 1))
   (if (bound-and-true-p focus-mode) "[x]" "[ ]"))
  ("v." (let ((width (read-from-minibuffer "width: " "")))
          (setq-local visual-fill-column-width (string-to-number width))
          (visual-fill-column-mode 1) (visual-line-mode 1))
   (if (bound-and-true-p visual-fill-column-width)
       visual-fill-column-width nil))
  ("q" nil "quit"))

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


;; Pseudo-hydras

(defun yxl-hydra-projects ()
  (interactive)
  (funcall
   (eval
    `(defhydra foobar (:color teal :columns 2
                              :pre (setq which-key-inhibit t)
                              :post (setq which-key-inhibit nil))
       "
Projects and folders
"
       ,@(mapcar (lambda (x)
                   (let ((path (car x))
                         (key (cdr x)))
                     (list key (append '(yxl-find-dir) (list path)) path)))
                 yxl-base-freq-projects-alist)
       ("." (ivy-read "Projects and folders: "
                      yxl-base-freq-projects-alist
                      :action (lambda (x) (yxl-find-dir (car x))))
        "ivy-read")))))

(defun yxl-hydra-files ()
  (interactive)
  (funcall
   (eval
    `(defhydra foobar (:color teal :columns 2
                              :pre (setq which-key-inhibit t)
                              :post (setq which-key-inhibit nil))
       "
Files
"
       ,@(mapcar (lambda (x)
                   (let ((path (car x))
                         (key (cdr x)))
                     (list key (append '(yxl-find-file-stay) (list path)) path)))
                 yxl-base-freq-files-alist)
       ("." (ivy-read "Files: "
                      yxl-base-freq-files-alist
                      :action (lambda (x) (yxl-find-file-stay (car x))))
        "ivy-read")))))
