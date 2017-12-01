(setq yxl-elfeed-packages '(elfeed
                            elfeed-goodies
                            elfeed-web
                            (yxl-elfeed :location site)))

(defun yxl-elfeed/init-elfeed ()
  (use-package elfeed
    :defer t
    :init (spacemacs/set-leader-keys "af" #'yxl-elfeed/invoke-elfeed)
    :commands (yxl-elfeed/invoke-elfeed)
    :config
    (progn
      (yxl-elfeed/setup-general-keybindings)
      (yxl-elfeed/setup-evilify-keybindings)
      (yxl-elfeed/setup-hydra)
      (add-hook 'elfeed-search-mode-hook #'yxl-elfeed/elfeed-search-mode-hook-config)
      (add-hook 'elfeed-show-mode-hook #'yxl-elfeed/elfeed-show-mode-hook-config)
      (add-hook 'elfeed-search-mode-hook #'yxl-big-text-mode)
      (add-hook 'elfeed-show-mode-hook #'yxl-big-text-mode)
      (setq elfeed-db-directory yxl-elfeed-db-directory)
      (setq elfeed-goodies/powerline-default-separator 'nil)
      (setq elfeed-goodies/entry-pane-position 'bottom)
      (setq elfeed-goodies/entry-pane-size 0.85))))

(defun yxl-elfeed/init-elfeed-goodies ()
  (use-package elfeed-goodies
    :commands elfeed-goodies/setup
    :init
    (spacemacs|use-package-add-hook elfeed
      :post-config
      (progn
        (elfeed-goodies/setup)
        (evil-define-key 'evilified elfeed-show-mode-map
          "o" 'elfeed-goodies/show-ace-link)))))

(defun yxl-elfeed/init-elfeed-web ()
  (use-package elfeed-web
    :defer t
    :commands elfeed-web-stop
    :init (when elfeed-enable-web-interface
            ;; TODO check if the port is already in use
            ;; hack to force elfeed feature to be required before elfeed-search
            (require 'elfeed)
            (elfeed-web-start))))

(defun yxl-elfeed/init-yxl-elfeed ()
  (use-package yxl-elfeed
    :defer t
    :after (elfeed)
    :config
    (progn
      (yxl-elfeed-patch)
      (setq elfeed-feeds yxl-personal-elfeed-feeds)
      (setq yxl-elfeed-score-alist yxl-personal-elfeed-score-alist))))
