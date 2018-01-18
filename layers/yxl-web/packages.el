(setq yxl-web-packages '(w3m
                         helm-w3m
                         sx
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
      (setq-default w3m-user-agent
                    (concat "Mozilla/5.0 (Linux; U;"
                            " Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40)"
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

(defun yxl-web/init-yxl-web ()
  (use-package yxl-web))

(defun yxl-web/init-atomic-chrome ()
  (use-package atomic-chrome
    :defer t))

(defun yxl-web/init-helm-chrome ()
  (use-package helm-chrome
    :defer t))
