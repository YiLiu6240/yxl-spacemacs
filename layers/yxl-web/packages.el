;;; config.el --- W3M Layer configuration File for Spacemacs
;;
;;
;; Original Author: Kuroi Mato <venmos@fuck.gfw.es>
;; URL: https://github.com/venmos/w3m-layer
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq yxl-web-packages '(w3m
                         helm-w3m
                         sx
                         elfeed
                         atomic-chrome))

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
                      nil nil "google.com" nil )))
  (w3m-goto-url
   (concat "http://" site)))

(defun v/w3m-open-site-new-session (site)
  "Opens site in new w3m session with 'http://' appended"
  (interactive
   (list (read-string "Enter website address (default: google.com):"
                      nil nil "google.com" nil )))
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
      (add-hook 'w3m-mode-hook (lambda () (setq yxl-line-width 100)))
      (setq-default w3m-user-agent (concat "Mozilla/5.0 (Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40)"
                                           " AppleWebKit/533.1 (KHTML, like Gecko)"
                                           " Version/4.0 Mobile Safari/533."))
      ;; REVIEW: this is bugged
      ;; w3m seems to auto break line when fetch html
      ;; (add-hook 'w3m-mode-hook (lambda () (visual-line-mode t)))
      (setq w3m-home-page "http://www.google.com"
            ;; W3M default display images
            w3m-default-display-inline-images t
            w3m-default-toggle-inline-images t
            ;; W3M use cookies
            w3m-command-arguments '("-cookie" "-F")
            w3m-use-cookies t
            ;; Browse url function use w3m
            ;; browse-url-browser-function 'w3m-browse-url
            ;; W3M view url new session in background
            w3m-view-this-url-new-session-in-background t)

      (evil-set-initial-state 'w3m-mode 'evilified)
      (evilified-state-evilify-map w3m-mode-map
        :mode w3m-mode
        :bindings
        "q" #'delete-window
        "t" #'v/w3m-open-site-new-session
        "x" #'w3m-session-select-quit
        "o" #'w3m-view-this-url
        "O" #'w3m-view-this-url-new-session
        (kbd "C-h") #'windmove-left
        (kbd "C-j") #'windmove-down
        (kbd "C-k") #'windmove-up
        (kbd "C-l") #'windmove-right
        "H" 'eyebrowse-prev-window-config
        "L" 'eyebrowse-next-window-config)
      (spacemacs/set-leader-keys-for-major-mode 'w3m-mode
        "gT" 'w3m-previous-buffer
        "gt" 'w3m-next-buffer
        "<" 'w3m-tab-move-left
        ">" 'w3m-tab-move-right
        "o" 'w3m-view-url-with-browse-url
        "x" 'w3m-session-select-quit)
      (define-key w3m-mode-map (kbd "C-f") 'evil-scroll-page-down)
      (define-key w3m-mode-map (kbd "C-b") 'evil-scroll-page-up))))

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
    (defalias 'elfeed-toggle-star
      (elfeed-expose #'elfeed-search-toggle-all 'star))
    (setq elfeed-goodies/powerline-default-separator 'nil)
    (yxl-web/elfeed-bindings)
    (yxl-web/elfeed-hydra-setup)
    (yxl-web/elfeed-patch)

    (defadvice elfeed-show-yank (after elfeed-show-yank-to-kill-ring activate compile)
      "Insert the yanked text from x-selection to kill ring"
      (kill-new (x-get-selection)))
    (ad-activate 'elfeed-show-yank)))

(defun yxl-web/init-atomic-chrome ()
  (use-package atomic-chrome
    :defer t))
