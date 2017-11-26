(setq yxl-email-packages
      '((mu4e :location site)
        mu4e-alert
        mu4e-maildirs-extension
        org))

(defun yxl-email/post-init-mu4e ()
  (with-eval-after-load 'mu4e
    (progn
      (setq mu4e-update-interval nil)
      (setq mu4e-compose-signature-auto-include nil)
      (setq mu4e-view-show-images t)
      (setq mu4e-view-show-addresses t)
      (load "~/dotfiles/email-configs/yxl-emacs-mu4e.el")
      ;; This is set externally
      (setq yxl-email/account-alist my-mu4e-account-alist)
      (add-hook 'mu4e-compose-pre-hook 'yxl-email/mu4e-set-account)
      (setq message-kill-buffer-on-exit t)
      (setq mu4e-context-policy 'pick-first)
      (setq mu4e-confirm-quit nil)
      (setq mu4e-enable-notifications t)))
  (with-eval-after-load 'mu4e-alert
    (progn
      (mu4e-alert-set-default-style 'notifications)
      (cond
       ((eq system-type 'gnu/linux)
        (mu4e-alert-set-default-style 'libnotify))
       ((eq system-type 'darwin)
        (mu4e-alert-set-default-style 'notifier))))))

(defun yxl-email/init-mu4e ()
  (use-package mu4e
    :commands (mu4e mu4e-compose-new)
    :init
    (progn
      (spacemacs/set-leader-keys "a M" 'mu4e)
      (global-set-key (kbd "C-x m") 'mu4e-compose-new))
    :config
    (progn
      ;; TODO leader key or hydras
      (evilified-state-evilify-map mu4e-main-mode-map
        :mode mu4e-main-mode
        :bindings
        (kbd "J") #'mu4e~headers-jump-to-maildir)

      (evilified-state-evilify-map
        mu4e-headers-mode-map
        :mode mu4e-headers-mode
        :bindings
        (kbd "C-j") #'mu4e-headers-next
        (kbd "C-k") #'mu4e-headers-prev
        (kbd "J") #'mu4e~headers-jump-to-maildir)

      (evilified-state-evilify-map
        mu4e-view-mode-map
        :mode mu4e-view-mode
        :bindings
        (kbd "C-j") #'mu4e-view-headers-next
        (kbd "C-k") #'mu4e-view-headers-prev
        (kbd "J") #'mu4e~headers-jump-to-maildir)

      (spacemacs/set-leader-keys-for-major-mode 'mu4e-compose-mode
        dotspacemacs-major-mode-leader-key 'message-send-and-exit
        "c" #'message-send-and-exit
        "k" #'message-kill-buffer
        "a" #'message-kill-buffer
        "s" #'message-dont-send         ; saves as draft
        "f" #'mml-attach-file)

      (setq mu4e-completing-read-function 'completing-read)

      (add-to-list 'mu4e-view-actions
                   '("View in browser" . mu4e-action-view-in-browser) t))))

(defun yxl-email/init-mu4e-alert ()
  (use-package mu4e-alert
    :defer t
    :init
    (with-eval-after-load 'mu4e
      (when mu4e-enable-notifications
        (mu4e-alert-enable-notifications))
      (when mu4e-enable-mode-line
        (mu4e-alert-enable-mode-line-display)))))

(defun yxl-email/init-mu4e-maildirs-extension ()
  (use-package mu4e-maildirs-extension
    :defer t
    :init (with-eval-after-load 'mu4e (mu4e-maildirs-extension-load))))

(defun yxl-email/pre-init-org ()
  ;; load org-mu4e when org is actually loaded
  (with-eval-after-load 'org
    (require 'org-mu4e nil 'noerror)
    (require 'org-notmuch nil 'noerror)))
