(defvar yxl-workspace-stored-config nil)

(spacemacs|define-transient-state window-manipulation
  :title "Window Manipulation Transient State"
  :doc (concat "
 Select^^^^              Move^^^^              Split^^                Resize^^                     Other^^
 ──────^^^^───────────── ────^^^^───────────── ─────^^─────────────── ──────^^──────────────────── ─────^^──────────────────────────────
 [_j_/_k_] down/up       [_J_/_K_] down/up     [_s_] vertical         [_[_] shrink horizontally    [_q_] quit
 [_h_/_l_] left/right    [_H_/_L_] left/right  [_S_] vert & follow    [_]_] enlarge horizontally   [_u_] restore prev layout
 [_0_-_9_] window N      [_r_]^^   rotate fwd  [_v_] horizontal       [_{_] shrink vertically      [_U_] restore next layout
 [_w_]^^   other window  [_R_]^^   rotate bwd  [_V_] horiz & follow   [_}_] enlarge vertically     [_d_] close current
 [_o_]^^   other frame   ^^^^                  ^^                     ^^                           [_D_] close other
 [_M-j_/_M-k_] move window down/up
 [_M-h_/_M-l_] move left/right"
               (if (configuration-layer/package-usedp 'golden-ratio)
                   "\n ^^^^                    ^^^^                  ^^                     ^^                           [_g_] golden-ratio %`golden-ratio-mode"
                 ""))
  :bindings
  ("q" nil :exit t)
  ("0" select-window-0)
  ("1" select-window-1)
  ("2" select-window-2)
  ("3" select-window-3)
  ("4" select-window-4)
  ("5" select-window-5)
  ("6" select-window-6)
  ("7" select-window-7)
  ("8" select-window-8)
  ("9" select-window-9)
  ("-" split-window-below-and-focus)
  ("/" split-window-right-and-focus)
  ("[" spacemacs/shrink-window-horizontally)
  ("]" spacemacs/enlarge-window-horizontally)
  ("{" spacemacs/shrink-window)
  ("}" spacemacs/enlarge-window)
  ("d" delete-window)
  ("D" delete-other-windows)
  ("h" evil-window-left)
  ("<left>" evil-window-left)
  ("j" evil-window-down)
  ("<down>" evil-window-down)
  ("k" evil-window-up)
  ("<up>" evil-window-up)
  ("l" evil-window-right)
  ("<right>" evil-window-right)
  ("H" evil-window-move-far-left)
  ("<S-left>" evil-window-move-far-left)
  ("J" evil-window-move-very-bottom)
  ("<S-down>" evil-window-move-very-bottom)
  ("K" evil-window-move-very-top)
  ("<S-up>" evil-window-move-very-top)
  ("L" evil-window-move-far-right)
  ("<S-right>" evil-window-move-far-right)
  ("M-h" buf-move-left)
  ("M-j" buf-move-down)
  ("M-k" buf-move-up)
  ("M-l" buf-move-right)
  ("o" other-frame)
  ("r" spacemacs/rotate-windows)
  ("R" spacemacs/rotate-windows-backward)
  ("s" split-window-below)
  ("S" split-window-below-and-focus)
  ("u" winner-undo)
  ("U" winner-redo)
  ("v" split-window-right)
  ("V" split-window-right-and-focus)
  ("w" other-window))

(defun yxl-workspace/setup-eyebrowse ()
  )

(defun eyebrowse-create-window-config-dired ()
  (interactive)
  (let* ((cur-buf-name (buffer-name (current-buffer)))
         (eyebrowse-new-workspace cur-buf-name))
    (call-interactively #'eyebrowse-create-window-config)
    (call-interactively #'dired-stay-or-jump)))

(defun eyebrowse-create-window-config-clone ()
  (interactive)
  (let* ((eyebrowse-new-workspace nil))
    (call-interactively 'eyebrowse-create-window-config)
    (message "clone to new workspace")))

(defun eyebrowse-create-window-config-main ()
  (interactive)
  (let* ((eyebrowse-new-workspace 'delete-other-windows))
    (call-interactively 'eyebrowse-create-window-config)
    (message "maximize window")))

(defun yxl-workspace/eyebrowse-populate-set1 ()
  "remove existing configs and
pre-populate/re-populate fake configs with names."
  (interactive)
  (mapcar 'eyebrowse--delete-window-config (number-sequence 0 9))
  (eyebrowse--insert-in-window-config-list
   (eyebrowse--current-window-config 0 "build"))
  (eyebrowse--insert-in-window-config-list
   (eyebrowse--current-window-config 1 "main"))
  (eyebrowse--insert-in-window-config-list
   (eyebrowse--current-window-config 2 "main2"))
  (eyebrowse--insert-in-window-config-list
   (eyebrowse--current-window-config 3 "items"))
  (eyebrowse--insert-in-window-config-list
   (eyebrowse--current-window-config 4 "items2"))
  (eyebrowse--insert-in-window-config-list
   (eyebrowse--current-window-config 5 "support"))
  (eyebrowse--insert-in-window-config-list
   (eyebrowse--current-window-config 6 "support2"))
  (eyebrowse--insert-in-window-config-list
   (eyebrowse--current-window-config 7 "doc"))
  (eyebrowse--insert-in-window-config-list
   (eyebrowse--current-window-config 8 "doc2"))
  (eyebrowse--insert-in-window-config-list
   (eyebrowse--current-window-config 9 "git")))

(defun yxl-workspace/record-config ()
  (interactive)
  (let ((window-configs (eyebrowse--get 'window-configs))
        (current-slot (eyebrowse--get 'current-slot))
        (last-slot (eyebrowse--get 'last-slot)))
    (setq yxl-workspace-stored-config `(,window-configs
                                        ,current-slot
                                        ,last-slot))))

(defun yxl-workspace/load-config ()
  (interactive)
  (let ((window-configs (nth 0 yxl-workspace-stored-config))
        (current-slot (nth 1 yxl-workspace-stored-config))
        (last-slot (nth 2 yxl-workspace-stored-config)))
    (if window-configs
        (progn
          (eyebrowse--set 'window-configs window-configs)
          (eyebrowse--set 'current-slot current-slot)
          (eyebrowse--set 'last-slot last-slot)
          (eyebrowse--load-window-config current-slot)))))
