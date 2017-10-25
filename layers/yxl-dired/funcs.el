(defun yxl-dired/general-config ()
  (setq line-spacing 4))

(defhydra yxl-dired-hydra-common (:color blue :hint nil)
  ("." nil "quit"))

(defhydra yxl-dired-hydra-mark
  (:hint nil :color red :inherit (yxl-dired-hydra-common/heads))
  "
 | _q_ ../         | _u_ unmark   |  _!_ unmark all | _s_ files in subdir
 | _m_ mark        | _@_ symlinks |  _/_ dirs       | ^^
 | _*_ executables | ^^           | ^^              | ^^
 | _t_ toggle      | ^^           | ^^              | ^^
 | _%_ regexp      | ^^           | ^^              | ^^
    "
  ("q" yxl-dired-hydra-main/body :color blue "../")
  ("m" dired-mark)
  ("*" dired-mark-executables)
  ("@" dired-mark-symlinks)
  ("/" dired-mark-directories)
  ("s" dired-mark-subdir-files)
  ("u" dired-unmark)
  ("!" dired-unmark-all-marks)
  ("t" dired-toggle-marks)
  ("%" dired-mark-files-regexp))

(defhydra yxl-dired-hydra-main
  (:color pink :inherit (yxl-dired-hydra-common/heads) :hint nil :columns 4)
  ("q" nil "quit" :color blue)
  ("o" yxl-dired-open-in-desktop "open in desktop" :color blue)
  ("s" hydra-dired-quick-sort/body "+sort" :color blue)
  ("T" yxl-dired-hydra-toggle/body "+toggle" :color blue)
  ("*" yxl-dired-hydra-mark/body "+mark" :color blue)
  ("e" ora-ediff-files "ediff" :color blue)
  ("h" yxl-dired-highlight-minor-mode "highlight" :color blue)
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
  ("z" yxl-dired-zip-files "zip")
  ("Z" dired-do-compress "compress/uncompress")
  ("_" xah-dired-rename-space-to-underscore "rename: _")
  ("-" xah-dired-rename-space-to-hyphen "rename: -")
  ("+" dired-create-directory "mkdir"))

(defhydra yxl-dired-hydra-toggle
  (:hint none :color red)
  "
 | _q_ ../             | _T_ ../      | _._ quit
 | _h_ ?h? hide detail | _H_ ?H? omit | _d_ ?d? dwim-target
 | _r_ read only (restore with C-x C-q)
"
  ("." nil)
  ("q" yxl-dired-hydra-main/body :color blue)
  ("T" yxl-dired-hydra-main/body :color blue)
  ("h" dired-hide-details-mode
   (if (bound-and-true-p dired-hide-details-mode)
       "[X]" "[ ]"))
  ("H" dired-omit-mode
   (if (bound-and-true-p dired-omit-mode)
       "[X]" "[ ]"))
  ("d" yxl-dired-toggle-dwim-target
   (if dired-dwim-target
       "[X]" "[ ]"))
  ("r" dired-toggle-read-only :color blue))

(defun yxl-dired/bindings-setup ()
  (define-key dired-mode-map "z" 'dired-zip-files)
  (define-key dired-mode-map "." 'yxl-dired-hydra-main/body)
  (define-key dired-mode-map (kbd "C-c \.") 'yxl-dired-hydra-main/body)
  (evilified-state-evilify dired-mode dired-mode-map
    "o" #'dired-find-file
    "q" #'yxl-dired-delete-window
    ;; from vinegar layer
    "0" #'dired-back-to-start-of-files
    "=" #'vinegar/dired-diff
    "I" #'vinegar/dotfiles-toggle
    ;; do not kill previous buffer, useful in splits
    (kbd "~") (lambda ()(interactive) (find-file "~/"))
    "T" #'dired-tree-down
    "f" (if (configuration-layer/layer-usedp 'ivy)
            #'counsel-find-file
          #'helm-find-files)
    "J" #'dired-goto-file
    (kbd "C-f") #'find-name-dired
    "K"         #'dired-do-kill-lines
    "r"         #'revert-buffer
    (kbd "C-r") #'dired-do-redisplay)
  (spacemacs/set-leader-keys-for-major-mode 'dired-mode
    "pp" #'peep-dired
    "pk" #'peep-dired-prev-file
    "pj" #'peep-dired-next-file
    "sn" #'yxl-dired/dired-sort-by-name
    "sd" #'yxl-dired/dired-sort-by-date
    "ss" #'yxl-dired/dired-sort-by-size
    "sD" #'yxl-dired/dired-sort-by-dir
    "td" #'yxl-dired-toggle-dwim-target
    "z"  #'yxl-dired-zip-files
    "R"  #'revert-buffer
    "h"  #'dired-hide-details-mode
    "H"  #'dired-omit-mode)
  (define-key dired-mode-map (kbd "C-c C-w") #'dired-toggle-read-only)
  (with-eval-after-load 'wdired
    (define-key wdired-mode-map (kbd "C-c C-w") #'wdired-finish-edit))
  (spacemacs/declare-prefix-for-mode #'dired-mode "ms" "sort")
  (spacemacs/declare-prefix-for-mode #'dired-mode "mt" "toggle")
  (spacemacs/declare-prefix-for-mode #'dired-mode "mp" "peep"))

(defun dired-copy-filename-as-kill-fullname ()
  (interactive)
  (dired-copy-filename-as-kill 0))

;; dired sort
;; http://ergoemacs.org/emacs/dired_sort.html
(defun yxl-dired/dired-sort-by-name ()
  (interactive)
  (setq -arg "-Al --si --time-style long-iso ")
  (dired-sort-other -arg))
(defun yxl-dired/dired-sort-by-date ()
  (interactive)
  (setq -arg "-Al --si --time-style long-iso -t")
  (dired-sort-other -arg))
(defun yxl-dired/dired-sort-by-size ()
  (interactive)
  (setq -arg "-Al --si --time-style long-iso -S")
  (dired-sort-other -arg))
(defun yxl-dired/dired-sort-by-dir ()
  (interactive)
  (setq -arg "-Al --si --time-style long-iso --group-directories-first")
  (dired-sort-other -arg))
