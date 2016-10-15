(defun yxl-dired/hydra-setup ()
  (defhydra hydra-dired-common (:color blue)
    ("." nil "quit"))
  (defhydra hydra-dired-mark
    (:hint nil :color red :inherit (hydra-dired-common/heads))
    "
    [_q_] ../
    [_m_] mark        [_u_] unmark   [_!_] unmark all
    [_*_] executables [_@_] symlinks [_/_] dirs       [_s_] files in subdir
    [_t_] toggle
    [_%_] regexp
    "
    ("q" hydra-dired-main/body :color blue "../")
    ("m" dired-mark)
    ("*" dired-mark-executables)
    ("@" dired-mark-symlinks)
    ("/" dired-mark-directories)
    ("s" dired-mark-subdir-files)
    ("u" dired-unmark)
    ("!" dired-unmark-all-marks)
    ("t" dired-toggle-marks)
    ("%" dired-mark-files-regexp))

  (defhydra hydra-dired-main
    (:hint nil :color red :inherit (hydra-dired-common/heads))
    "
    [_q_] quit      [___] ../    [_h_/_j_/_k_/_l_] navigation
    [_s_] sort+     [_T_] toggle+ [_*_] mark+
    [_m_] mark      [_u_] unmark  [_U_] unmark all
    [_w_] copy name
    [_C_] cp        [_D_] rm      [_R_] mv         [_H_] ln    [_S_] ln-s
    [_M_] chmod     [_G_] chgrp   [_O_] chown
    [_Z_] compress
    "
    ("q" nil :color blue)
    ("h" evil-backward-char)
    ("j" evil-next-visual-line)
    ("k" evil-previous-visual-line)
    ("l" evil-forward-char)
    ("_" dired-up-directory)
    ("s" hydra-dired-quick-sort/body :color blue)
    ("T" hydra-dired-toggle/body :color blue)
    ("*" hydra-dired-mark/body :color blue)
    ("m" dired-mark)
    ("u" dired-unmark)
    ("U" dired-unmark-all-marks)
    ("w" dired-copy-filename-as-kill :color blue)
    ("C" dired-do-copy)
    ("D" dired-do-delete)
    ("R" dired-do-rename)
    ("H" dired-do-hardlink)
    ("S" dired-do-symlink)
    ("M" dired-do-chmod)
    ("G" dired-do-chgrp)
    ("O" dired-do-chown)
    ("Z" dired-do-compress))

  (defhydra hydra-dired-toggle
    (:hint none :color red)
    "
    [_q_] ../   [_T_] ../   [_._] quit
    [_h_] hide detail
    [_r_] read only (restore with C-x C-q)
    [_d_] ?d? dwim-target"
    ("." nil)
    ("q" hydra-dired-main/body :color blue)
    ("T" hydra-dired-main/body :color blue)
    ("h" dired-hide-details-mode)
    ("d" yxl-dired/toggle-dwim-target
     (if (eq dired-dwim-target t)
         "[X]" "[ ]"))
    ("r" dired-toggle-read-only :color blue))

  (define-key dired-mode-map "." 'hydra-dired-main/body)
  (define-key dired-mode-map (kbd "C-c \.") 'hydra-dired-main/body))
