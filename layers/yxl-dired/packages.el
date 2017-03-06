(setq yxl-dired-packages '(dired
                           (yxl-dired :location site)
                           peep-dired
                           image+
                           dired-quick-sort
                           dired-subtree))

(defun yxl-dired/post-init-dired ()
  (use-package dired
    :defer t
    :config
    (progn
      ;; use homebrew coreutils in darwin
      ;; http://qiita.com/maangie/items/5a80ae50c13d14368a72
      (if (eq system-type 'darwin)
          (setq insert-directory-program "gls"))
      (with-eval-after-load 'dired-aux
        (add-to-list 'dired-compress-file-suffixes
                     '("\\.zip\\'" ".zip" "unzip")))
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
      (setq dired-recursive-copies 'always)
      (add-hook 'dired-mode-hook #'spacemacs/toggle-truncate-lines-on)
      (add-hook 'dired-mode-hook #'dired-hide-details-mode)
      (add-hook 'dired-mode-hook #'yxl-dired/general-config)
      (yxl-dired/bindings-setup))))

(defun yxl-dired/init-yxl-dired ()
  (use-package yxl-dired
    :after dired
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'dired-mode
        "oo" #'yxl-dired-open-aw
        "oO" #'dired-find-file-other-window
        "os" #'yxl-dired-open-aw-horz
        "ov" #'yxl-dired-open-aw-vert)
      (evilified-state-evilify dired-mode dired-mode-map
        "Oo" #'yxl-dired-open-aw
        "OO" #'dired-find-file-other-window
        "Os" #'yxl-dired-open-aw-horz
        "Ov" #'yxl-dired-open-aw-vert))))

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

(defun yxl-dired/init-dired-subtree ()
  (use-package dired-subtree
    :defer t
    :init
    (progn
      (with-eval-after-load 'dired
       (define-key dired-mode-map "i" #'dired-subtree-toggle)))
    :config
    (progn
      (setq dired-subtree-use-backgrounds nil))))
