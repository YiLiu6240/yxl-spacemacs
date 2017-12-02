(defun yxl-email/mu4e-setup-general-keybindings ()
  (global-set-key (kbd "C-x m") 'mu4e-compose-new)
  (define-key mu4e-main-mode-map (kbd "o") (kbd "RET"))
  (define-key mu4e-headers-mode-map
    "o" #'mu4e-headers-view-message)
  (define-key mu4e-view-mode-map
    "\'" #'yxl-email/mu4e-view-detach-to-win-or-frame))

(defun yxl-email/mu4e-setup-evilified-keybindings ()
  (evilified-state-evilify-map mu4e-main-mode-map
    :mode mu4e-main-mode
    :bindings
    (kbd "b") #'mu4e-headers-search-bookmark
    (kbd "a") #'mu4e~headers-jump-to-maildir)
  (evilified-state-evilify-map
    mu4e-headers-mode-map
    :mode mu4e-headers-mode
    :bindings
    (kbd "b") #'mu4e-headers-search-bookmark
    (kbd "J") #'mu4e-headers-next
    (kbd "K") #'mu4e-headers-prev
    (kbd "a") #'mu4e~headers-jump-to-maildir)
  (evilified-state-evilify-map
    mu4e-view-mode-map
    :mode mu4e-view-mode
    :bindings
    (kbd "J") #'mu4e-view-headers-next
    (kbd "K") #'mu4e-view-headers-prev
    (kbd "a") #'mu4e~headers-jump-to-maildir))

(defun yxl-email/mu4e-setup-leader-keys ()
  (spacemacs/declare-prefix-for-mode 'mu4e-main-mode
    "mu" "update")
  (spacemacs/set-leader-keys-for-major-mode 'mu4e-main-mode
    "uu" #'mu4e-maildirs-extension-force-update
    "uU" #'mu4e-update-mail-and-index
    "uq" #'yxl-email/mu4e-offlineimap-quick
    "up" #'yxl-email/mu4e-offlineimap-quick-profile
    ";" #'mu4e-context-switch
    "a" #'mu4e~headers-jump-to-maildir)
  (spacemacs/declare-prefix-for-mode 'mu4e-headers-mode
    "mu" "update")
  (spacemacs/set-leader-keys-for-major-mode 'mu4e-headers-mode
    "uu" #'mu4e-maildirs-extension-force-update
    "uU" #'mu4e-update-mail-and-index
    "uq" #'yxl-email/mu4e-offlineimap-quick
    "up" #'yxl-email/mu4e-offlineimap-quick-profile
    "C" #'mu4e-compose-new
    "R" #'mu4e-compose-reply
    "F" #'mu4e-compose-forward
    ";" #'mu4e-context-switch
    "a" #'mu4e~headers-jump-to-maildir)
  (spacemacs/set-leader-keys-for-major-mode 'mu4e-compose-mode
    dotspacemacs-major-mode-leader-key 'message-send-and-exit
    "c" #'message-send-and-exit
    "k" #'message-kill-buffer
    "a" #'message-kill-buffer
    "s" #'message-dont-send             ; saves as draft
    "f" #'mml-attach-file))
