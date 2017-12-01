(defun yxl-elfeed/setup-general-keybindings ()
  (unbind-key "b" elfeed-search-mode-map)
  (unbind-key "b" elfeed-show-mode-map)
  (evil-define-key 'visual elfeed-search-mode-map "m" #'yxl-elfeed-mark-as-read)
  (evil-define-key 'visual elfeed-search-mode-map "M" #'yxl-elfeed-mark-as-unread)
  (evil-define-key 'visual elfeed-search-mode-map "+" #'yxl-elfeed-add-tag)
  (evil-define-key 'visual elfeed-search-mode-map "-" #'yxl-elfeed-rm-tag))

(defun yxl-elfeed/setup-evilify-keybindings ()
  (evilified-state-evilify-map elfeed-search-mode-map
    :mode elfeed-search-mode
    :eval-after-load elfeed-search
    :bindings
    "*" #'elfeed-toggle-star
    "o" #'elfeed-search-show-entry
    "O" #'yxl-elfeed/elfeed-search-browse-url-w3m
    "x" #'elfeed-search-browse-url
    "m" #'yxl-elfeed-mark-as-read
    "M" #'yxl-elfeed-mark-as-unread
    "q" #'quit-window
    "+" #'yxl-elfeed-add-tag
    "-" #'yxl-elfeed-rm-tag
    "s" #'yxl-elfeed-helm-search
    "S" #'elfeed-search-set-filter)
  (evilified-state-evilify-map elfeed-show-mode-map
    :mode elfeed-show-mode
    :eval-after-load elfeed-show
    :bindings
    "q"           #'delete-window
    "O"           #'yxl-elfeed/elfeed-show-visit-w3m
    "x"           #'elfeed-show-visit
    "u"           #'elfeed-show-visit
    "J"           #'elfeed-show-next
    "K"           #'elfeed-show-prev
    (kbd "C-n")   #'elfeed-show-next
    (kbd "C-p")   #'elfeed-show-prev
    (kbd "TAB")   #'shr-next-link
    (kbd "S-TAB") #'shr-previous-link))

(defun yxl-elfeed/setup-leader-keys ()
  (spacemacs/declare-prefix-for-mode 'elfeed-search-mode
    "t" "toggle")
  (spacemacs/set-leader-keys-for-major-mode 'elfeed-search-mode
    "gr" #'elfeed-update
    "a"  #'elfeed-search-update--force
    "A"  #'elfeed-update
    "d"  #'elfeed-unjam
    "s"  #'yxl-elfeed-helm-search
    "S"  #'elfeed-search-set-filter
    "x"  #'elfeed-search-browse-url
    "y"  #'elfeed-search-yank
    "v"  #'set-mark-command
    "r"  #'elfeed-search-untag-all-unread
    "u"  #'elfeed-search-tag-all-unread
    "*"  #'elfeed-toggle-star
    "+"  #'yxl-elfeed-add-tag
    "-"  #'yxl-elfeed-rm-tag
    "w"  #'elfeed-web-start
    "W"  #'elfeed-web-stop
    "tm" #'elfeed-toggle-shr-inhibit-images)
  (spacemacs/declare-prefix-for-mode 'elfeed-show-mode
    "t" "toggle")
  (spacemacs/set-leader-keys-for-major-mode 'elfeed-show-mode
    "g"  #'elfeed-show-refresh
    "x"  #'elfeed-show-visit
    "J"  #'elfeed-show-next
    "K"  #'elfeed-show-prev
    "s"  #'elfeed-show-new-live-search
    "O"  #'yxl-elfeed/show-visit-w3m
    "y"  #'elfeed-show-yank
    "u"  #'yxl-elfeed/show-tag-unread
    "*"  #'elfeed-toggle-star
    "+"  #'elfeed-show-tag
    "-"  #'elfeed-show-untag
    "tm" #'elfeed-toggle-shr-inhibit-images))
