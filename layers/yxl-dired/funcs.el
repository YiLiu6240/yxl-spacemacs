(defun yxl-dired/bindings-setup ()
  (define-key dired-mode-map "z" 'dired-zip-files)
  (define-key dired-mode-map "." 'hydra-dired-main/body)
  (define-key dired-mode-map (kbd "C-c \.") 'hydra-dired-main/body)
  (spacemacs/set-leader-keys-for-major-mode 'dired-mode
    "pp" #'peep-dired
    "pk" #'peep-dired-prev-file
    "pj" #'peep-dired-next-file
    "sn" #'yxl-dired/dired-sort-by-name
    "sd" #'yxl-dired/dired-sort-by-date
    "ss" #'yxl-dired/dired-sort-by-size
    "sD" #'yxl-dired/dired-sort-by-dir
    "td" #'yxl-dired/toggle-dwim-target
    "o"  #'yxl-dired/open-in-desktop
    "r"  #'revert-buffer
    "h"  #'dired-hide-details-mode
    "H"  #'dired-omit-mode)
  (spacemacs/declare-prefix-for-mode #'dired-mode "ms" "sort")
  (spacemacs/declare-prefix-for-mode #'dired-mode "mt" "toggle")
  (spacemacs/declare-prefix-for-mode #'dired-mode "mp" "peep")
  (defhydra hydra-dired-common (:color blue)
    ("." nil "quit"))
  (defhydra hydra-dired-mark
    (:hint nil :color red :inherit (hydra-dired-common/heads))
    "
 | _q_ ../         | _u_ unmark   |  _!_ unmark all | _s_ files in subdir
 | _m_ mark        | _@_ symlinks |  _/_ dirs       | ^^
 | _*_ executables | ^^           | ^^              | ^^
 | _t_ toggle      | ^^           | ^^              | ^^
 | _%_ regexp      | ^^           | ^^              | ^^
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
    (:color pink :inherit (hydra-dired-common/heads))
    "
 | _T_: +toggle ^^^^                   | _s_: +sort  ^^^^               | _*_: +mark ^^^^                 |
 | _m_/_u_/_U_: mark/unmark/unmark all | _y_: filename ^^^^             | _C_/_D_/_R_: copy/delete/rename | _+_: mkdir |
 | _H_/_S_: hardlink/symlink ^^        | _M_/_G_/_O_: chmod/chgrp/chown |
 | _Z_: compress ^^^^                  |
 | ___/_-_: rename: _/- ^^             |
"
    ("q" nil "quit" :color blue)
    ("o" yxl-dired/open-in-desktop "open in desktop" :color blue)
    ("s" hydra-dired-quick-sort/body "+sort" :color blue)
    ("T" hydra-dired-toggle/body "+toggle" :color blue)
    ("*" hydra-dired-mark/body "+mark" :color blue)
    ("m" dired-mark "mark")
    ("u" dired-unmark "unmark")
    ("U" dired-unmark-all-marks "unmark all")
    ("y" dired-copy-filename-as-kill "filename" :color blue)
    ("Y" dired-copy-filename-as-kill-fullname "full filename" :color blue)
    ("C" dired-do-copy "copy")
    ("D" dired-do-delete "delete")
    ("R" dired-do-rename "rename")
    ("H" dired-do-hardlink "hardlink")
    ("S" dired-do-symlink "symlink")
    ("M" dired-do-chmod "chmod")
    ("G" dired-do-chgrp "chgrp")
    ("O" dired-do-chown "chown")
    ("Z" dired-do-compress "compress")
    ("_" xah-dired-rename-space-to-underscore "rename: _")
    ("-" xah-dired-rename-space-to-hyphen "rename: -")
    ("+" dired-create-directory "mkdir"))
  (defhydra hydra-dired-toggle
    (:hint none :color red)
    "
 | _q_ ../             | _T_ ../      | _._ quit
 | _h_ ?h? hide detail | _H_ ?H? omit | _d_ ?d? dwim-target
 | _r_ read only (restore with C-x C-q)
"
    ("." nil)
    ("q" hydra-dired-main/body :color blue)
    ("T" hydra-dired-main/body :color blue)
    ("h" dired-hide-details-mode
     (if (bound-and-true-p dired-hide-details-mode)
         "[X]" "[ ]"))
    ("H" dired-omit-mode
     (if (bound-and-true-p dired-omit-mode)
         "[X]" "[ ]"))
    ("d" yxl-dired/toggle-dwim-target
     (if dired-dwim-target
         "[X]" "[ ]"))
    ("r" dired-toggle-read-only :color blue)))

(defun dired-copy-filename-as-kill-fullname ()
  (interactive)
  (dired-copy-filename-as-kill 0))

(defun yxl-dired/open-in-desktop ()
  "Show current file in desktop (OS's file manager).
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-11-30"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
   ((string-equal system-type "darwin") (shell-command "open ."))
   ((string-equal system-type "gnu/linux")
    (let (
          (process-connection-type nil)
          (openFileProgram (if (file-exists-p "/usr/bin/gvfs-open")
                               "/usr/bin/gvfs-open"
                             "/usr/bin/xdg-open")))
      (start-process "" nil openFileProgram ".")))))

(defun yxl-dired/toggle-dwim-target ()
  "toggle the value of dired-dwim-target."
  (interactive)
  (if (equal dired-dwim-target t)
      (setq dired-dwim-target nil)
    (setq dired-dwim-target t))
  (message "dired-dwim-target: %s" dired-dwim-target))
