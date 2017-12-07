(setq yxl-elfeed-packages '(elfeed
                            elfeed-goodies
                            elfeed-web
                            (yxl-elfeed :location site)))

(defun yxl-elfeed/init-elfeed ()
  (use-package elfeed
    :defer t
    :init (spacemacs/set-leader-keys "af" #'elfeed)
    :config
    (progn
      (yxl-elfeed/setup-general-keybindings)
      (yxl-elfeed/setup-evilify-keybindings)
      (yxl-elfeed/setup-leader-keys)
      (add-hook 'elfeed-search-mode-hook
                #'yxl-elfeed/elfeed-search-mode-hook-config)
      (add-hook 'elfeed-show-mode-hook
                #'yxl-elfeed/elfeed-show-mode-hook-config)
      (setq elfeed-goodies/powerline-default-separator 'nil)
      (setq elfeed-goodies/entry-pane-position 'bottom)
      (setq elfeed-goodies/entry-pane-size 0.85)
      (setq elfeed-enable-web-interface t)
      (setq spacemacs-useful-buffers-regexp
            (append spacemacs-useful-buffers-regexp '("\\*elfeed-search\\*")))
      (when yxl-elfeed-db-directory
        (setq elfeed-db-directory yxl-elfeed-db-directory))
      (when yxl-elfeed-personal-config-file
        (load yxl-elfeed-personal-config-file)))))

(defun yxl-elfeed/init-elfeed-goodies ()
  (use-package elfeed-goodies
    :commands elfeed-goodies/setup
    :init
    (spacemacs|use-package-add-hook elfeed
      :post-config
      (progn
        (elfeed-goodies/setup)
        (evil-define-key 'evilified elfeed-show-mode-map
          "o" #'elfeed-goodies/show-ace-link)))))

(defun yxl-elfeed/init-elfeed-web ()
  (use-package elfeed-web
    :defer t))

(defun yxl-elfeed/init-yxl-elfeed ()
  (use-package yxl-elfeed
    :after (elfeed)
    :config
    (progn
      (yxl-elfeed-patch))))
