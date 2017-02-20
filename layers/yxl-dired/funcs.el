(defun yxl-dired/bindings-setup ()
  (define-key dired-mode-map "z" 'dired-zip-files)
  (define-key dired-mode-map "." 'yxl-dired-hydra-main/body)
  (define-key dired-mode-map (kbd "C-c \.") 'yxl-dired-hydra-main/body)
  (spacemacs/set-leader-keys-for-major-mode 'dired-mode
    "pp" #'peep-dired
    "pk" #'peep-dired-prev-file
    "pj" #'peep-dired-next-file
    "sn" #'yxl-dired/dired-sort-by-name
    "sd" #'yxl-dired/dired-sort-by-date
    "ss" #'yxl-dired/dired-sort-by-size
    "sD" #'yxl-dired/dired-sort-by-dir
    "td" #'yxl-dired-toggle-dwim-target
    "o"  #'yxl-open-in-desktop
    "z"  #'yxl-dired-zip-files
    "r"  #'revert-buffer
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
