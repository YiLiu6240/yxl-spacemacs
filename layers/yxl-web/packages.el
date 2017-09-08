(setq yxl-web-packages '(w3m
                         helm-w3m
                         sx
                         elfeed
                         ;; (yxl-elfeed :location site)
                         (yxl-web :location site)
                         atomic-chrome
                         helm-chrome))

(defun yxl-web/init-helm-w3m ()
  "Initializes helm-w3m and adds keybindings for its exposed functionalities."
  (use-package helm-w3m
    :commands (helm-w3m-bookmarks)
    :init
    (progn
      (spacemacs/set-leader-keys
        "awb" 'helm-w3m-bookmarks))))

(defun v/w3m-open-site (site)
  "Opens site in new w3m session with 'http://' appended"
  (interactive
   (list (read-string "Enter website address (default: google.com):"
                      nil nil "google.com" nil)))
  (w3m-goto-url
   (concat "http://" site)))

(defun v/w3m-open-site-new-session (site)
  "Opens site in new w3m session with 'http://' appended"
  (interactive
   (list (read-string "Enter website address (default: google.com):"
                      nil nil "google.com" nil)))
  (w3m-goto-url-new-session
   (concat "http://" site)))

(defun w3m-hackernews ()
  (interactive)
  (w3m-goto-url-new-session "http://news.ycombinator.com"))

(defun w3m-reddit (subreddit)
  (interactive (list
                (read-string "Enter subreddit (default: emacs):"
                             nil nil "emacs" nil)))
  (w3m-goto-url-new-session (format "http://m.reddit.com/r/%s" subreddit)))

(defun yxl-web/init-w3m()
  "Initializes w3m and adds keybindings for its exposed functionalities."
  (use-package w3m
    :defer t
    :commands (v/w3m-open-site
               w3m-goto-url
               w3m-goto-url-new-session
               w3m-search
               w3m-search-new-session)
    :init
    (progn
      (spacemacs/declare-prefix "aw" "w3m")
      (spacemacs/set-leader-keys
        "awo" 'v/w3m-open-site
        "aww" 'w3m
        "awg" 'w3m-goto-url
        "awG" 'w3m-goto-url-new-session
        "aws" 'w3m-search
        "awS" 'w3m-search-new-session
        "awr" 'w3m-reddit
        "awh" 'w3m-hackernews))
    :config
    (progn
      ;; (add-hook 'w3m-mode-hook (lambda () (setq yxl-line-width 100)))
      ;; (add-hook 'w3m-mode-hook #'yxl-big-text-mode)
      (setq-default w3m-user-agent (concat "Mozilla/5.0 (Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40)"
                                           " AppleWebKit/533.1 (KHTML, like Gecko)"
                                           " Version/4.0 Mobile Safari/533."))
      (setq w3m-home-page "http://www.google.com")
      (setq w3m-default-display-inline-images nil)
      (setq w3m-default-toggle-inline-images t)
      (setq w3m-command-arguments '("-cookie" "-F"))
      (setq w3m-use-cookies t)
      (setq w3m-view-this-url-new-session-in-background t)
      ;; (setq w3m-fill-column 100)
      (yxl-web/w3m-bindings)
      (yxl-web/w3m-hydra))))

(defun yxl-web/init-sx ()
  (use-package sx
    :defer t
    :init
    (bind-keys :prefix "C-c s"
               :prefix-map my-sx-map
               :prefix-docstring "Global keymap for SX."
               ("q" . sx-tab-all-questions)
               ("i" . sx-inbox)
               ("o" . sx-open-link)
               ("u" . sx-tab-unanswered-my-tags)
               ("a" . sx-ask)
               ("s" . sx-search))))

(defun yxl-web/post-init-elfeed ()
  (with-eval-after-load 'elfeed
    (add-hook 'elfeed-search-mode-hook #'yxl-web/elfeed-search-mode-config)
    (add-hook 'elfeed-show-mode-hook #'yxl-web/elfeed-show-mode-config)
    (add-hook 'elfeed-search-mode-hook #'yxl-big-text-mode)
    (add-hook 'elfeed-show-mode-hook #'yxl-big-text-mode)
    (setq elfeed-db-directory "~/Dropbox/rss/.elfeed")
    (setq elfeed-goodies/powerline-default-separator 'nil)
    (setq elfeed-goodies/entry-pane-position 'bottom)
    (setq elfeed-goodies/entry-pane-size 0.85)
    (yxl-web/elfeed-bindings)
    (yxl-web/elfeed-hydra-setup)
    (spacemacs/set-leader-keys "af" #'yxl-web/invoke-elfeed)))

(defun yxl-web/init-yxl-elfeed ()
  (use-package yxl-elfeed
    :defer t
    :after (elfeed)
    :config
    (progn
      (yxl-elfeed-patch)
      (setq elfeed-feeds yxl-personal-elfeed-feeds)
      (setq yxl-elfeed-score-alist yxl-personal-elfeed-score-alist))))

(defun yxl-web/init-yxl-web ()
  (use-package yxl-web))

(defun yxl-web/init-atomic-chrome ()
  (use-package atomic-chrome
    :defer t))

(defun yxl-web/init-helm-chrome ()
  (use-package helm-chrome
    :defer t))
