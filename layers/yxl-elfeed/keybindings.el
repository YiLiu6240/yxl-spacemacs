(defun yxl-elfeed/setup-general-keybindings ()
  (unbind-key "b" elfeed-search-mode-map)
  (unbind-key "b" elfeed-show-mode-map)
  (define-key elfeed-search-mode-map "." 'hydra-elfeed-search/body)
  (define-key elfeed-show-mode-map "." 'hydra-elfeed-show/body)
  (evil-define-key 'visual elfeed-search-mode-map "m" #'yxl-elfeed-mark-as-read)
  (evil-define-key 'visual elfeed-search-mode-map "M" #'yxl-elfeed-mark-as-unread)
  (evil-define-key 'visual elfeed-search-mode-map "+" #'yxl-elfeed-add-tag)
  (evil-define-key 'visual elfeed-search-mode-map "-" #'yxl-elfeed-rm-tag))

(defun yxl-elfeed/setup-evilify-keybindings ()
  (evilified-state-evilify-map elfeed-search-mode-map
    :mode elfeed-search-mode
    :eval-after-load elfeed-search
    :bindings
    "c"  'elfeed-db-compact
    "gr" 'elfeed-update
    "gR" 'elfeed-search-update--force
    "gu" 'elfeed-unjam
    "o"  'elfeed-load-opml
    "q"  'quit-window
    "w"  'elfeed-web-start
    "W"  'elfeed-web-stop)
  (evilified-state-evilify-map elfeed-search-mode-map
    :mode elfeed-search-mode
    :eval-after-load elfeed-search
    :bindings
    "*" #'elfeed-toggle-star
    "c" nil
    "gr" nil
    "gR" nil
    "gu" nil
    "o" #'elfeed-search-show-entry
    "O" #'yxl-elfeed/elfeed-search-browse-url-w3m
    "x" #'elfeed-search-browse-url
    ;; ",R" #'zilong/elfeed-mark-all-as-read
    "m" #'yxl-elfeed-mark-as-read
    "M" #'yxl-elfeed-mark-as-unread
    ;; ",tm" #'elfeed-toggle-shr-inhibit-images
    "q"  #'quit-window
    (kbd "C-h") #'windmove-left
    (kbd "C-j") #'windmove-down
    (kbd "C-k") #'windmove-up
    (kbd "C-l") #'windmove-right
    (kbd "g C-h") #'eyebrowse-prev-window-config
    (kbd "g C-l") #'eyebrowse-next-window-config
    "+" #'yxl-elfeed-add-tag
    "-" #'yxl-elfeed-rm-tag
    "w"  nil
    "W"  nil
    "s" #'yxl-elfeed-helm-search
    "S" #'elfeed-search-set-filter)
  (evilified-state-evilify-map elfeed-show-mode-map
    :mode elfeed-show-mode
    :eval-after-load elfeed-show
    :bindings
    "q" 'quit-window
    (kbd "C-j") 'elfeed-show-next
    (kbd "C-k") 'elfeed-show-prev)
  (evilified-state-evilify-map elfeed-show-mode-map
    :mode elfeed-show-mode
    :eval-after-load elfeed-show
    :bindings
    "q" #'delete-window
    ;; ",tm" #'elfeed-toggle-shr-inhibit-images
    ;; ",O" #'elfeed-search-browse-url
    "O"     #'yxl-elfeed/elfeed-show-visit-w3m
    "x"     #'elfeed-show-visit
    "u"     #'elfeed-show-visit
    "J"     #'elfeed-show-next
    "K"     #'elfeed-show-prev
    (kbd "C-h") #'windmove-left
    (kbd "C-j") #'windmove-down
    (kbd "C-k") #'windmove-up
    (kbd "C-l") #'windmove-right
    (kbd "g C-h") #'eyebrowse-prev-window-config
    (kbd "g C-l") #'eyebrowse-next-window-config
    (kbd "C-n") #'elfeed-show-next
    (kbd "C-p") #'elfeed-show-prev))

(defun yxl-elfeed/setup-hydra ()
  ;; https://github.com/joedicastro/dotfiles/blob/master/emacs/.emacs.d/init.el
  (defhydra hydra-elfeed-common (:color blue)
    ;; ("\\" hydra-master/body "back")  ; NOTE: this is currently not set
    ("." nil "quit"))

  (defhydra hydra-elfeed-search
    (:hint nil :color blue :inherit (hydra-elfeed-common/heads))
    "
 | _k_:  up  | _s_: filter (helm)    | _o_: view          | _a_: refresh    |
 | _j_: down | _S_: filter (nonlive) | _O_: browser (w3m) | _A_: fetch      |
 | ^^        | _c_: clean filter     | ^^                 | ^^              |
 | ^^        | _r_: read             | _x_: browser (gen) | _w_: db-save    |
 | ^^        | _u_: unread           | _y_: yank url      | _d_: unjam      |
 | ^^        | _+_: add              | _v_: mark          | _E_: edit feeds |
 | ^^        | _-_: remove           | ^^                 | _q_: exit       |
    "
    ("q"    quit-window)
    ("a"    elfeed-search-update--force)
    ("A"    elfeed-update)
    ("d"    elfeed-unjam)
    ("s"    yxl-elfeed-helm-search)
    ("S"    elfeed-search-set-filter)
    ("c"    (elfeed--read-tag (default-value 'elfeed-search-filter)))
    ("RET"  elfeed-search-show-entry)
    ("o"    elfeed-search-show-entry)
    ("O"    yxl-elfeed/elfeed-search-browse-url-w3m)
    ("x"    elfeed-search-browse-url)
    ("y"    elfeed-search-yank)
    ("v"    set-mark-command)
    ("j"    next-line :color red)
    ("k"    previous-line :color red)
    ("r"    elfeed-search-untag-all-unread)
    ("u"    elfeed-search-tag-all-unread)
    ("E"    (lambda() (interactive) (find-file "~/dotfiles/rss/feeds.org")))
    ("*"    elfeed-toggle-star :color red)
    ("+"    yxl-elfeed-add-tag)
    ("-"    yxl-elfeed-rm-tag)
    ("w"    (elfeed-db-save)))

  (defhydra hydra-elfeed-show
    (:hint nil :color blue :inherit (hydra-elfeed-common/heads))
    "
 | _k_:  up  |   _g_: refresh       | _u_: unread | _S-TAB_ |
 | _j_: down |   _O_: browser (w3m) | _+_: add    | ^  ↑  ^ |
 | ^^        |   _x_: browser (gen) | ^^          | ^     ^ |
 | _K_: prev |   _y_: yank url      | _-_: remove | ^     ^ |
 | _J_: next |   _q_: quit          | _*_: star   | ^  ↓  ^ |
 | ^^        |   _s_: quit & search | ^^          |  _TAB_  |
    "
    ("q"     elfeed-kill-buffer)
    ("g"     elfeed-show-refresh)
    ("j"     evil-next-visual-line :color red)
    ("k"     evil-previous-visual-line :color red)
    ("J"     elfeed-show-next :color red)
    ("K"     elfeed-show-prev :color red)
    ("s"     elfeed-show-new-live-search)
    ("O"     yxl-elfeed/elfeed-show-visit-w3m :color red)
    ("x"     elfeed-show-visit :color red)
    ("y"     elfeed-show-yank)
    ("u"     (elfeed-show-tag 'unread))
    ("*"     elfeed-toggle-star)
    ("+"     elfeed-show-tag)
    ("-"     elfeed-show-untag)
    ("TAB"   shr-next-link :color red)
    ("S-TAB" shr-previous-link :color red)))
