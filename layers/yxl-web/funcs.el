(defun yxl-web/elfeed-search-mode-config ()
  (setq shr-inhibit-images t)
  (setq line-spacing 4))

(defun yxl-web/elfeed-show-mode-config ()
  (setq shr-inhibit-images t)
  (setq line-spacing 4)
  (setq yxl-line-width 100)
  (visual-line-mode t))

(defun yxl-web/elfeed-search-browse-url-w3m ()
  "inject w3m to be the browser function"
  (interactive)
  (let* ((browse-url-browser-function #'w3m-goto-url-new-session))
    (elfeed-search-browse-url)))

(defun yxl-web/elfeed-show-visit-w3m ()
  "inject w3m to be the browser function"
  (interactive)
  (let* ((browse-url-browser-function #'w3m-goto-url-new-session))
    (elfeed-show-visit)))

(defun yxl-web/elfeed-bindings ()
  (unbind-key "b" elfeed-search-mode-map)
  (unbind-key "b" elfeed-show-mode-map)

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
    ;; "O" #'elfeed-search-browse-url
    "O" #'yxl-web/elfeed-search-browse-url-w3m
    "u" #'elfeed-search-browse-url
    ;; ",R" #'zilong/elfeed-mark-all-as-read
    "m" #'yxl-elfeed-mark-as-read
    "M" #'yxl-elfeed-mark-as-unread
    ;; ",tm" #'elfeed-toggle-shr-inhibit-images
    "q"  #'quit-window
    (kbd "C-h") #'windmove-left
    (kbd "C-j") #'windmove-down
    (kbd "C-k") #'windmove-up
    (kbd "C-l") #'windmove-right
    "H" #'eyebrowse-prev-window-config
    "L" #'eyebrowse-next-window-config
    "w"  nil
    "W"  nil)

  (evilified-state-evilify-map elfeed-show-mode-map
    :mode elfeed-show-mode
    :eval-after-load elfeed-show
    :bindings
    "q" #'quit-window
    ;; ",tm" #'elfeed-toggle-shr-inhibit-images
    ;; ",O" #'elfeed-search-browse-url
    "O"     #'yxl-web/elfeed-show-visit-w3m
    "u"     #'elfeed-show-visit
    "J"     #'elfeed-show-next
    "K"     #'elfeed-show-prev
    (kbd "C-h") #'windmove-left
    (kbd "C-j") #'windmove-down
    (kbd "C-k") #'windmove-up
    (kbd "C-l") #'windmove-right
    "H" #'eyebrowse-prev-window-config
    "L" #'eyebrowse-next-window-config
    (kbd "C-n") #'elfeed-show-next
    (kbd "C-p") #'elfeed-show-prev))

(defun yxl-web/elfeed-hydra-setup ()
  ;; https://github.com/joedicastro/dotfiles/blob/master/emacs/.emacs.d/init.el
  (defhydra hydra-elfeed-common (:color blue)
    ;; ("\\" hydra-master/body "back")  ; NOTE: this is currently not set
    ("." nil "quit"))

  (defhydra hydra-elfeed-search
    (:hint nil :color blue :inherit (hydra-elfeed-common/heads))
    "
--------------------------------------------------------------------------------
  [_k_]  up     [_s_] live   [_RET_] view     [_r_] read      [_a_] refresh
  [_j_] down    [_S_] set    [_o_] browse     [_u_] unread    [_A_] fetch
   ^ ^           ^ ^         [_y_] yank url   [_+_] add       [_d_] unjam
   ^ ^           ^ ^         [_v_] mark       [_-_] remove    [_E_] edit feeds
   ^ ^           ^ ^          ^ ^              ^ ^            [_q_] exit
--------------------------------------------------------------------------------
    "
    ("q"    quit-window)
    ("a"    elfeed-search-update--force)
    ("A"    elfeed-update)
    ("d"    elfeed-unjam)
    ("s"    elfeed-search-live-filter)
    ("S"    elfeed-search-set-filter)
    ("RET"  elfeed-search-show-entry)
    ("o"    elfeed-search-show-entry)
    ("O"    elfeed-search-browse-url)
    ("y"    elfeed-search-yank)
    ("v"    set-mark-command)
    ("j"    next-line :color red)
    ("k"    previous-line :color red)
    ("r"    elfeed-search-untag-all-unread)
    ("u"    elfeed-search-tag-all-unread)
    ("E"    (lambda() (interactive) (find-file "~/dotfiles/rss/feeds.org")))
    ("*"    elfeed-toggle-star :color red)
    ("+"    elfeed-search-tag-all)
    ("-"    elfeed-search-untag-all))

  (defhydra hydra-elfeed-show
    (:hint nil :color blue :inherit (hydra-elfeed-common/heads))
    "
--------------------------------------------------------------------------------
   [_k_]  up     [_g_] refresh   [_u_] unread    _S-TAB_
   [_j_] down    [_O_] browse    [_+_] add       ^  ↑  ^
   [_K_] prev    [_y_] yank url  [_-_] remove    ^     ^
   [_J_] next    [_q_] quit      [_*_] star      ^  ↓  ^
    ^ ^          [_s_] quit & search^^            _TAB_
--------------------------------------------------------------------------------
    "
    ("q"     elfeed-kill-buffer)
    ("g"     elfeed-show-refresh)
    ("j"     evil-next-visual-line :color red)
    ("k"     evil-previous-visual-line :color red)
    ("J"     elfeed-show-next :color red)
    ("K"     elfeed-show-prev :color red)
    ("s"     elfeed-show-new-live-search)
    ("O"     elfeed-show-visit :color red)
    ("y"     elfeed-show-yank)
    ("u"     (elfeed-show-tag 'unread))
    ("*"     elfeed-toggle-star)
    ("+"     elfeed-show-tag)
    ("-"     elfeed-show-untag)
    ("TAB"   shr-next-link :color red)
    ("S-TAB" shr-previous-link :color red))

  (define-key elfeed-search-mode-map "." 'hydra-elfeed-search/body)
  (define-key elfeed-show-mode-map "." 'hydra-elfeed-show/body))
