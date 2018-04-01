(defun w3m-reddit (subreddit)
  (interactive (list
                (read-string "Enter subreddit (default: emacs):"
                             nil nil "emacs" nil)))
  (w3m-goto-url-new-session (format "http://m.reddit.com/r/%s" subreddit)))


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


(defun yxl-web/w3m-open-external ()
  (interactive)
  (browse-url w3m-current-url))

(defun yxl-web/w3m-bindings ()
  (evil-set-initial-state 'w3m-mode 'evilified)
  (evilified-state-evilify-map w3m-mode-map
    :mode w3m-mode
    :bindings
    "t" #'v/w3m-open-site-new-session
    "x" #'yxl-web/w3m-open-external
    "o" #'w3m-view-this-url
    "O" #'w3m-view-this-url-new-session
    (kbd "C-h") #'windmove-left
    (kbd "C-j") #'windmove-down
    (kbd "C-k") #'windmove-up
    (kbd "C-l") #'windmove-right
    (kbd "g C-h") 'eyebrowse-prev-window-config
    (kbd "g C-l") 'eyebrowse-next-window-config)
  (define-key w3m-mode-map (kbd "C-f") 'evil-scroll-page-down)
  (define-key w3m-mode-map (kbd "C-b") 'evil-scroll-page-up)
  (define-key w3m-mode-map "." 'hydra-w3m/body))

(defun yxl-web/w3m-hydra ()
  (defhydra hydra-w3m (:color blue)
    ("q" delete-window "quit")
    ("H" w3m-previous-buffer "prev tab")
    ("L" w3m-next-buffer "next tab")
    ("t" w3m-toggle-inline-image "toggle image")
    ("T" (lambda ()
           (interactive)
           (w3m-toggle-inline-images)
           (message "w3m-display-inline-images: %s" w3m-display-inline-images))
     "toggle images in page")
    (">" w3m-tab-move-right "mv tab right")
    ("<" w3m-tab-move-left "mv tab left")
    ("o" w3m-view-url-with-browse-url "open")
    ("x" w3m-session-select-quit "kill")))
