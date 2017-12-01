(setq yxl-email-packages
      '((mu4e :location site)
        mu4e-alert
        mu4e-maildirs-extension
        org))

(defun yxl-email/init-mu4e ()
  (use-package mu4e
    :commands (mu4e mu4e-compose-new)
    :init
    (progn
      (spacemacs/set-leader-keys "a M" 'mu4e))
    :config
    (progn
      (yxl-email/mu4e-setup-general-keybindings)
      (yxl-email/mu4e-setup-evilified-keybindings)
      (yxl-email/mu4e-setup-leader-keys)
      (setq mu4e-completing-read-function 'completing-read)
      (setq mu4e-update-interval nil)
      (setq mu4e-compose-signature-auto-include nil)
      (setq mu4e-view-show-images t)
      (setq mu4e-view-show-addresses t)
      (setq message-kill-buffer-on-exit t)
      (setq mu4e-context-policy 'pick-first)
      (setq mu4e-confirm-quit nil)
      (setq mu4e-enable-notifications t)
      (setq mu4e-headers-date-format "%a %Y-%m-%d %H:%M")
      (setq mu4e-headers-fields
            '((:human-date . 20)
              (:flags . 6)
              (:mailing-list . 10)
              (:from . 22)
              (:subject)))
      (add-hook 'mu4e-compose-mode-hook (lambda () (auto-fill-mode -1)))
      (add-to-list 'mu4e-view-actions
                   '("View in browser" . mu4e-action-view-in-browser) t)
      (when yxl-email-personal-config-file
        (load yxl-email-personal-config-file)))))

(defun yxl-email/init-mu4e-alert ()
  (use-package mu4e-alert
    :defer t
    :init
    (with-eval-after-load 'mu4e
      (when mu4e-enable-notifications
        (mu4e-alert-enable-notifications))
      (when mu4e-enable-mode-line
        (mu4e-alert-enable-mode-line-display))
      (mu4e-alert-set-default-style 'notifications)
      (cond
       ((eq system-type 'gnu/linux)
        (mu4e-alert-set-default-style 'libnotify))
       ((eq system-type 'darwin)
        (mu4e-alert-set-default-style 'notifier))))))

(defun yxl-email/init-mu4e-maildirs-extension ()
  (use-package mu4e-maildirs-extension
    :defer t
    :init (with-eval-after-load 'mu4e (mu4e-maildirs-extension-load))
    :config
    (progn
      (setq mu4e-maildirs-extension-toggle-maildir-key (kbd "<tab>")))))

(defun yxl-email/pre-init-org ()
  ;; load org-mu4e when org is actually loaded
  (with-eval-after-load 'org
    (require 'org-mu4e nil 'noerror)
    (require 'org-notmuch nil 'noerror)))
