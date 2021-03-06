(defun yxl-dired/general-config ()
  (setq line-spacing 4))

(defun yxl-dired/open-split-or-aw ()
  "If there is only one window in the current frame, open file at point in a split;
else open file at point using ace-window.

With a C-u prefix, always open using ace-window.
"
  (interactive)
  (if (or (> (count-windows) 1)
          current-prefix-arg)
      (call-interactively #'yxl-dired-open-aw)
    (progn
      (split-window-sensibly)
      (dired-find-file))))

(defun yxl-dired/bindings-setup ()
  (define-key dired-mode-map "z" 'dired-zip-files)
  (define-key dired-mode-map "." 'yxl-dired/hydra-main/body)
  (define-key dired-mode-map "," 'yxl-dired/hydra-main/body)
  (define-key dired-mode-map (kbd "C-c \.") 'yxl-dired/hydra-main/body)
  (evilified-state-evilify-map dired-mode-map
    :mode dired-mode
    :bindings
    "o" #'dired-find-file
    "O" nil
    "OO" #'yxl-dired/open-split-or-aw
    "OS" #'yxl-dired-open-aw-horz
    "OV" #'yxl-dired-open-aw-vert
    "q" #'yxl-dired-delete-window
    "-" #'dired-jump
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
  (define-key dired-mode-map (kbd "C-c C-w") #'dired-toggle-read-only)
  (with-eval-after-load 'wdired
    (define-key wdired-mode-map (kbd "C-c C-w") #'wdired-finish-edit))
  (with-eval-after-load 'hydra
    (defhydra yxl-dired/hydra-common (:color blue :hint nil)
      ("." nil "quit"))
    (defhydra yxl-dired/hydra-mark
      (:hint nil :color red :inherit (yxl-dired/hydra-common/heads))
      "
 | _q_ ../         | _u_ unmark   |  _!_ unmark all | _s_ files in subdir
 | _m_ mark        | _@_ symlinks |  _/_ dirs       | ^^
 | _*_ executables | ^^           | ^^              | ^^
 | _t_ toggle      | ^^           | ^^              | ^^
 | _%_ regexp      | ^^           | ^^              | ^^
    "
      ("q" yxl-dired/hydra-main/body "../" :color blue)
      ("m" dired-mark)
      ("*" dired-mark-executables)
      ("@" dired-mark-symlinks)
      ("/" dired-mark-directories)
      ("s" dired-mark-subdir-files)
      ("u" dired-unmark)
      ("!" dired-unmark-all-marks)
      ("t" dired-toggle-marks)
      ("%" dired-mark-files-regexp))
    (defhydra yxl-dired/hydra-main
      (:color pink :inherit (yxl-dired/hydra-common/heads)
              :pre (setq which-key-inhibit t)
              :post (setq which-key-inhibit nil)
              :hint nil :columns 4)
      ("q" nil "quit" :color blue)
      ("dm" dired-mark "mark")
      ("du" dired-unmark "unmark")
      ("dU" dired-unmark-all-marks "unmark all")
      ("dy" dired-copy-filename-as-kill "filename" :color blue)
      ("dY" dired-copy-filename-as-kill-fullname "full filename" :color blue)
      ("dC" dired-do-copy "copy")
      ("dD" dired-do-delete "delete")
      ("dR" dired-do-rename "rename")
      ("dH" dired-do-hardlink "hardlink")
      ("dS" dired-do-symlink "symlink")
      ("dM" dired-do-chmod "chmod")
      ("dG" dired-do-chgrp "chgrp")
      ("dO" dired-do-chown "chown")
      ("dz" yxl-dired-zip-files "zip")
      ("dZ" dired-do-compress "compress/uncompress")
      ("fo" yxl-open-file-external "open in external" :color blue)
      ("fe" ora-ediff-files "ediff" :color blue)
      ("f_" xah-dired-rename-space-to-underscore "rename: _")
      ("f-" xah-dired-rename-space-to-hyphen "rename: -")
      ("fy" (let ((filename (dired-filename-at-point)))
              (kill-new filename)
              (message "Yank %s => kill-ring" filename))
       "yank name: long" :color blue)
      ("fY" (let ((filename (file-name-nondirectory (dired-filename-at-point))))
              (kill-new filename)
              (message "Yank %s => kill-ring" filename))
       "yank name: short" :color blue)
      ("h" yxl-dired-highlight-minor-mode "highlight" :color blue)
      ("OO" yxl-dired/open-split-or-aw "dired-open-aw")
      ("OS" yxl-dired-open-aw-horz "dired-open-aw-horz")
      ("OV" yxl-dired-open-aw-vert "dired-open-aw-vert")
      ("pp" peep-dired "peep-dired")
      ("pk" peep-dired-prev-file "peep-dired-prev-file")
      ("pj" peep-dired-next-file "peep-dired-next-file")
      ("rc" dired-ranger-copy "dired-ranger-copy")
      ("ry" dired-ranger-copy "dired-ranger-copy")
      ("rx" dired-ranger-move "dired-ranger-move")
      ("rp" dired-ranger-paste "dired-ranger-paste")
      ("sS" hydra-dired-quick-sort/body "+sort" :color blue)
      ("sn" yxl-dired/dired-sort-by-name "sort-by-name" :color blue)
      ("sd" yxl-dired/dired-sort-by-date "sort-by-date" :color blue)
      ("ss" yxl-dired/dired-sort-by-size "sort-by-size" :color blue)
      ("sD" yxl-dired/dired-sort-by-dir "sort-by-dir" :color blue)
      ("+" dired-create-directory "mkdir")
      ("T" yxl-dired/hydra-toggle/body "+toggle" :color blue)
      ("*" yxl-dired/hydra-mark/body "+mark" :color blue))
    (defhydra yxl-dired/hydra-toggle
      (:hint none :color red)
      "
 | _q_ ../             | _T_ ../      | _._ quit
 | _h_ ?h? hide detail | _H_ ?H? omit | _d_ ?d? dwim-target
 | _r_ read only (restore with C-x C-q)
"
      ("." nil)
      ("q" yxl-dired/hydra-main/body :color blue)
      ("T" yxl-dired/hydra-main/body :color blue)
      ("h" dired-hide-details-mode
       (if (bound-and-true-p dired-hide-details-mode)
           "[X]" "[ ]"))
      ("H" dired-omit-mode
       (if (bound-and-true-p dired-omit-mode)
           "[X]" "[ ]"))
      ("d" yxl-dired-toggle-dwim-target
       (if dired-dwim-target
           "[X]" "[ ]"))
      ("r" dired-toggle-read-only :color blue))))

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

(defun dired-ranger-Downloads-paste (arg)
  "Hack `dired-ranger-paste' to paste to \"~/Downloads/\".
There is no need to call `dired-ranger-copy' first."
  (interactive "P")
  (call-interactively #'dired-ranger-copy)
  (let* ((index (if (numberp arg) arg 0))
         (data (ring-ref dired-ranger-copy-ring index))
         (files (cdr data))
         (target-directory (expand-file-name "~/Downloads/"))
         (copied-files 0))
    (--each files (when (file-exists-p it)
                    (if (file-directory-p it)
                        (copy-directory it target-directory)
                      (condition-case err
                          (copy-file it target-directory 0)
                        (file-already-exists nil)))
                    (cl-incf copied-files)))
    (dired-ranger--revert-target ?P target-directory files)
    (unless arg (ring-remove dired-ranger-copy-ring 0))
    (message (format "Pasted %d/%d item%s from copy ring."
                     copied-files
                     (length files)
                     (if (> (length files) 1) "s" "")))))

(defun dired-ranger-Downloads-move (arg)
  (interactive "P")
  (call-interactively #'dired-ranger-copy)
  (let* ((index (if (numberp arg) arg 0))
         (data (ring-ref dired-ranger-copy-ring index))
         (buffers (car data))
         (files (cdr data))
         (target-directory (expand-file-name "~/Downloads/"))
         (copied-files 0))
    (--each files (when (file-exists-p it)
                    (condition-case err
                        (rename-file it target-directory 0)
                      (file-already-exists nil))
                    (cl-incf copied-files)))
    (dired-ranger--revert-target ?M target-directory files)
    (--each buffers
      (when (buffer-live-p it)
        (with-current-buffer it (revert-buffer))))
    (unless arg (ring-remove dired-ranger-copy-ring 0))
    (message (format "Moved %d/%d item%s from copy ring."
                     copied-files
                     (length files)
                     (if (> (length files) 1) "s" "")))))
