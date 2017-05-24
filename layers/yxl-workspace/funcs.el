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
  (defvar spacemacs--ts-full-hint-toggle 0
    "Toggle display of transient states documentations.")
  (defun spacemacs//workspaces-ts-toggle-hint ()
    "Toggle the full hint docstring for the workspaces transient-state."
    (interactive)
    (setq spacemacs--ts-full-hint-toggle
          (logxor spacemacs--ts-full-hint-toggle 1)))

  (defun spacemacs/workspaces-ts-rename ()
    "Rename a workspace and get back to transient-state."
    (interactive)
    (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) nil)
    (spacemacs/workspaces-transient-state/body))
  (defun spacemacs//workspace-format-name (workspace)
    "Return a propertized string given a WORKSPACE name."
    (let* ((current (eq (eyebrowse--get 'current-slot) (car workspace)))
           (name (nth 2 workspace))
           (number (car workspace))
           (caption (if (< 0 (length name))
                        (concat (int-to-string number) ":" name)
                      (int-to-string number))))
      (if current
          (propertize (concat "[" caption "]") 'face 'warning)
        caption)))
  (defun spacemacs//workspaces-ts-hint ()
    "Return a one liner string containing all the workspaces names."
    (concat
     " "
     (mapconcat 'spacemacs//workspace-format-name
                (eyebrowse--get 'window-configs) " | ")
     (if (equal 1 spacemacs--ts-full-hint-toggle)
         spacemacs--workspaces-ts-full-hint
       (concat "  (["
               (propertize "?" 'face 'hydra-face-red)
               "] help)"))))
  (spacemacs|transient-state-format-hint workspaces
    spacemacs--workspaces-ts-full-hint
    "\n\n
 Go to^^^^^^                         Actions^^
 ─────^^^^^^───────────────────────  ───────^^──────────────────────
 [_0_,_9_]^^     nth/new workspace   [_._] switch
 [_C-0_,_C-9_]^^ nth/new workspace   [_?_] toggle help
 [_<tab>_]^^^^   last workspace      [_R_] rename current workspace
 [_l_/_C-l_]^^   next workspace      [_d_] close current workspace
 [_h_/_C-h_]^^   prev workspace      [_c_] create new workspace
                                     [_C_] clone to new workspace
                                     [_C-c_] clone main config")
  (spacemacs|define-transient-state workspaces
        :title "Workspaces Transient State"
        :hint-is-doc t
        :dynamic-hint (spacemacs//workspaces-ts-hint)
        :bindings
        ("." eyebrowse-switch-to-window-config)
        ("?" spacemacs//workspaces-ts-toggle-hint)
        ("0" eyebrowse-switch-to-window-config-0 :exit t)
        ("1" eyebrowse-switch-to-window-config-1 :exit t)
        ("2" eyebrowse-switch-to-window-config-2 :exit t)
        ("3" eyebrowse-switch-to-window-config-3 :exit t)
        ("4" eyebrowse-switch-to-window-config-4 :exit t)
        ("5" eyebrowse-switch-to-window-config-5 :exit t)
        ("6" eyebrowse-switch-to-window-config-6 :exit t)
        ("7" eyebrowse-switch-to-window-config-7 :exit t)
        ("8" eyebrowse-switch-to-window-config-8 :exit t)
        ("9" eyebrowse-switch-to-window-config-9 :exit t)
        ("C-0" eyebrowse-switch-to-window-config-0)
        ("C-1" eyebrowse-switch-to-window-config-1)
        ("C-2" eyebrowse-switch-to-window-config-2)
        ("C-3" eyebrowse-switch-to-window-config-3)
        ("C-4" eyebrowse-switch-to-window-config-4)
        ("C-5" eyebrowse-switch-to-window-config-5)
        ("C-6" eyebrowse-switch-to-window-config-6)
        ("C-7" eyebrowse-switch-to-window-config-7)
        ("C-8" eyebrowse-switch-to-window-config-8)
        ("C-9" eyebrowse-switch-to-window-config-9)
        ("<tab>" eyebrowse-last-window-config)
        ("C-h" eyebrowse-prev-window-config)
        ("C-i" eyebrowse-last-window-config)
        ("C-l" eyebrowse-next-window-config)
        ("h" eyebrowse-prev-window-config :exit t)
        ("l" eyebrowse-next-window-config :exit t)
        ("c" eyebrowse-cwc-dired)
        ("C" eyebrowse-cwc-clone)
        ("C-c" eyebrowse-cwc-main)
        ("d" eyebrowse-close-window-config)
        ("R" spacemacs/workspaces-ts-rename :exit t)
        ("w" eyebrowse-switch-to-window-config :exit t)))

(defun eyebrowse-cwc-dired ()
  (interactive)
  (let* ((cur-buf-name (buffer-name (current-buffer)))
         (eyebrowse-new-workspace cur-buf-name))
    (call-interactively #'eyebrowse-create-window-config)
    (call-interactively #'dired-stay-or-jump)))

(defun eyebrowse-cwc-clone ()
  (interactive)
  (let* ((eyebrowse-new-workspace nil))
    (call-interactively 'eyebrowse-create-window-config)
    (message "clone to new workspace")))

(defun eyebrowse-cwc-main ()
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

(defun yxl-workspace/setup-eyebrowse-keys ()
  (let ((map eyebrowse-mode-map))
    (define-key map (kbd "C-c w C-h") 'eyebrowse-prev-window-config)
    (define-key map (kbd "C-c w C-l") 'eyebrowse-next-window-config)
    (define-key map (kbd "C-c w d") 'eyebrowse-close-window-config)
    ;; eyebrowse new window config:
    ;; c: jump to current dired
    ;; C: clone current window config
    ;; C-c: new config with current window maximized
    (define-key map (kbd "C-c w c") 'eyebrowse-cwc-dired)
    (define-key map (kbd "C-c w C") 'eyebrowse-cwc-clone)
    (define-key map (kbd "C-c w C-c") 'eyebrowse-cwc-main)))
