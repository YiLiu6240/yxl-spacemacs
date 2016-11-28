(setq yxl-dired-packages '(dired
                           (dired-goodies :location local)
                           peep-dired
                           image+
                           dired-quick-sort))

(defun yxl-dired/post-init-dired ()
  (use-package dired
    :defer t
    :config
    (progn
      ;; use homebrew coreutils in darwin
      ;; http://qiita.com/maangie/items/5a80ae50c13d14368a72
      (if (eq system-type 'darwin)
          (setq insert-directory-program "gls"))

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

      (setq dired-listing-switches "-l -a -h")
      (add-hook 'dired-mode-hook #'spacemacs/toggle-truncate-lines-on)
      (add-hook 'dired-mode-hook #'dired-hide-details-mode)

      (spacemacs/set-leader-keys-for-major-mode 'dired-mode
        "pp" #'peep-dired
        "pk" #'peep-dired-prev-file
        "pj" #'peep-dired-next-file
        "sn" #'yxl-dired/dired-sort-by-name
        "sd" #'yxl-dired/dired-sort-by-date
        "ss" #'yxl-dired/dired-sort-by-size
        "sD" #'yxl-dired/dired-sort-by-dir
        "td" #'yxl-dired/toggle-dwim-target
        "o" #'yxl-dired/open-in-desktop
        "r" #'revert-buffer
        "i" #'dired-hide-details-mode)
      (spacemacs/declare-prefix-for-mode #'dired-mode "ms" "sort")
      (spacemacs/declare-prefix-for-mode #'dired-mode "mt" "toggle")
      (spacemacs/declare-prefix-for-mode #'dired-mode "mp" "peep")

      (yxl-dired/hydra-setup)

      (spacemacs/set-leader-keys
        "obo" #'yxl-dired/open-in-desktop))))

(defun yxl-dired/init-dired-goodies ()
  (use-package dired-goodies
    :after dired))

(defun yxl-dired/init-image+ ()
  (use-package image+
    :defer t
    :commands (imagex-global-sticky-mode
               imagex-auto-adjust-mode)
    :init
    (progn
      (imagex-global-sticky-mode)
      (imagex-auto-adjust-mode)
      (with-eval-after-load 'image-mode
        ;; (add-hook 'image-mode-hook #'imagex-sticky-mode)
        ;; (add-hook 'image-mode-hook #'imagex-auto-adjust-mode)
        (spacemacs/set-leader-keys-for-major-mode 'image-mode
          "=" #'imagex-sticky-zoom-in
          "+" #'imagex-sticky-zoom-in
          "-" #'imagex-sticky-zoom-out
          "M" #'imagex-sticky-maximize
          "O" #'imagex-sticky-restore-original
          "S" #'imagex-sticky-save-image
          "r" #'imagex-sticky-rotate-right
          "l" #'imagex-sticky-rotate-left)))))

(defun yxl-dired/init-peep-dired ()
  ;;preview files in dired
  (use-package peep-dired
    :defer t
    :commands (peep-dired-next-file
               peep-dired-prev-file)
    :bind (:map dired-mode-map
                ("P" . peep-dired))))

; TODO: eval this
;; (use-package peep-dired
;;   :bind (:map peep-dired-mode-map
;;               ("SPC" . nil)
;;               ("<backspace>" . nil)))

(defun yxl-dired/init-dired-quick-sort ()
  (use-package dired-quick-sort
    :defer t
    :init
    (with-eval-after-load 'dired
      (dired-quick-sort-setup))))
