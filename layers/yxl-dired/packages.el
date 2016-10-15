(defconst yxl-dired-packages
  '(dired
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
