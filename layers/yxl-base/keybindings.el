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
                                    :post (setq which-key-inhibit nil)
                                    :columns 4)
  "
Hotspot:

Orgmode:
 | [_oo_]: ivy-read agenda    | [_oO_]: open all files     | ^^                 |
 | [_oc_]: capture            | [_ol_]: org-store-link     | ^^                 |
 | [_oa_]: my-org-agenda-life | [_oA_]: my-org-agenda-work | [_oL_]: my-org-log |

Projects and files:
 | [_op_]: projects           | [_of_]: files              | ^^                 |

Applications:
 | [_aa_]: invoke apps        | [_ac_]: cfw                | [_aC_]: calendar   |

Rest:"

  ("oo" (ivy-read "org-agenda-files:"
                  (org-agenda-files)
                  :action (lambda (file)
                            (find-file file))))
  ("oO" yxl-org-open-all-task-files)
  ("oc" org-capture)
  ("ol" org-store-link)
  ("oa" my-org-agenda-life)
  ("oA" my-org-agenda-work)
  ("oL" my-org-log)

  ("op" yxl-hydra-projects)
  ("of" yxl-hydra-files)

  ("aa" yxl-invoke-applications)
  ("ac" cfw/open-calendar)
  ("aC" calendar)

  ("is" (yxl-append-to-scratch yxl-base-org-todo-life) "Append to org today")
  ("ia" yxl-append-to-scratch "Append to scratch buffer"))

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
 | [_Su_]: update Spacemacs | [_SU_]: update packages | [_SR_]: roll back packages
 | [_d_]:  +display-format  |
 | [_tw_]: switch browser   | [_tb_]: big text
--------
"

  ("." make-frame "make-frame")
  ("<RET>" make-frame "make-frame")
  ("," #'set-frame-name "set-frame-name")
  (";" eval-expression "eval-expression")

  ("D" #'delete-frame  "delete-frame")

  ("-" yxl-dired-popup "dired-popup")
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

  ("sf" #'spacemacs/scale-font-transient-state/body)
  ("sF" #'yxl-hydra-frame-size/body)
  ("d" #'yxl-hydra-display-format/body)

  ("tw" yxl-web-switch-browser)
  ("tm" menu-bar-mode)
  ("tb" yxl-big-text-mode))

(defhydra yxl-hydra-frame-size (:color red :hint nil)
  ("+" yxl-ui/frame-zoom-in "zoom-in")
  ("=" yxl-ui/frame-zoom-in "zoom-in")
  ("-" yxl-ui/frame-zoom-out "zoom-out")
  ("0" spacemacs/zoom-frm-unzoom "reset")
  ("1" (yxl-ui/frame-zoom-state 4) "profile 1")
  ("2" (yxl-ui/frame-zoom-state 6) "profile 2")
  ("3" (yxl-ui/frame-zoom-state 8) "profile 3")
  ("4" (yxl-ui/frame-zoom-state 12) "profile 4"))

(defhydra yxl-hydra-display-format (:color amaranth :hint nil
                                           :pre (setq which-key-inhibit t)
                                           :post (setq which-key-inhibit nil))
  "
Display format:
line-spacing: %(or line-spacing 0)
visual-fill-column-width: %`visual-fill-column-width
font-scale: %(or (when (featurep 'face-remap) text-scale-mode-amount) 0.0)
frame-scale: %(or (frame-parameter nil 'zoomed) 0)
------------
[_v_]: ?v? visual-line-mode
[_f_]: ?f? visual-fill-column-mode
[_c_]: ?c? visual-fill-column-center-tex
[_C_]: ?C? focus-mode
[_j_/_k_]: Decrease / Increase line-spacing
[_J_/_K_]: Decrease / Increase font size
[_-_/_=_]: Decrease / Increase frame scale
[_sf_/_sF_]: +font-scale / Frame-scale
[_T_]: +Transparency
[_._]: visual-fill-column-width
------------
"
  ("v" visual-line-mode
   (if visual-line-mode "[x]" "[ ]"))
  ("f" (if visual-fill-column-mode
           (progn (visual-fill-column-mode -1))
         (progn
           (visual-fill-column-mode 1)
           (visual-line-mode 1)))
   (if (bound-and-true-p visual-fill-column-mode) "[x]" "[ ]"))
  ("c" (progn (setq visual-fill-column-center-text
                    (not visual-fill-column-center-text))
         (visual-fill-column-mode 1) (visual-line-mode 1))
   (if (bound-and-true-p visual-fill-column-center-text) "[x]" "[ ]"))
  ("C" (if focus-mode
           (focus-mode -1)
         (focus-mode 1))
   (if (bound-and-true-p focus-mode) "[x]" "[ ]"))
  ("k" (if line-spacing
           (setq line-spacing (1+ line-spacing))
         (setq line-spacing 1)))
  ("j" (if line-spacing
            (if (= 0 line-spacing)
                (setq line-spacing nil)
              (setq line-spacing (1- line-spacing)))
          (setq line-spacing 0)))
  ("K" spacemacs/scale-up-font)
  ("J" spacemacs/scale-down-font)
  ("+" yxl-ui/frame-zoom-in)
  ("=" yxl-ui/frame-zoom-in)
  ("-" yxl-ui/frame-zoom-out)
  ("sf" (spacemacs/scale-font-transient-state/body) :color blue)
  ("sF" (yxl-hydra-frame-size/body) :color blue)
  ("T" #'spacemacs/scale-transparency-transient-state/spacemacs/toggle-transparency)
  ("." (let ((width (read-from-minibuffer "width: " "")))
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
