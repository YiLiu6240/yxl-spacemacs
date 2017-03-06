(defun yxl-dired/general-config ()
  (setq line-spacing 4))

(defun yxl-dired/bindings-setup ()
  (define-key dired-mode-map "z" 'dired-zip-files)
  (define-key dired-mode-map "." 'yxl-dired-hydra-main/body)
  (define-key dired-mode-map (kbd "C-c \.") 'yxl-dired-hydra-main/body)
  (evilified-state-evilify dired-mode dired-mode-map
    "o" #'dired-find-file
    "q" #'yxl-dired-delete-window
    "-" #'dired-up-directory
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
