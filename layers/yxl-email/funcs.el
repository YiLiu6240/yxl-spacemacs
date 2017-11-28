(defun yxl-email/mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account-alist yxl-email/account-alist)  ;; this is set externally
         (account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field
                              mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                account-alist "/"))
                             (mapcar #'(lambda (var) (car var))
                                     account-alist)
                             nil t nil nil (caar account-alist))))
         (account-vars (cdr (assoc account account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

(defun yxl-email/mu4e-offlineimap-quick ()
  "Use `offlineimap -o -q' as the command to update mail."
  (interactive)
  (let ((mu4e-get-mail-command "offlineimap -o -q"))
    (call-interactively #'mu4e-update-mail-and-index)))

(defun yxl-email/mu4e-offlineimap-quick-profile ()
  "Use `offlineimap -o -q -a <profile>' as the command to update mail.
<profile> uses the variable `offlineimap-profile'."
  (interactive)
  (let ((mu4e-get-mail-command (format "offlineimap -o -q -a %s" offlineimap-profile)))
    (call-interactively #'mu4e-update-mail-and-index)))

(defun yxl-email/mu4e-view-detach-to-win-or-frame (&optional toframe focusnew)
  "Detach the current mu4e-view buffer from header to a new
splitted window or a new frame.

If toframe is t, the detached message view will be presented in a
new frame. Otherwise it will be presented in a splitted window.

If focusnew is t, the new window/frame will be focused.

Source: https://github.com/djcb/mu/issues/905"
  (interactive)
  (let* ((buf (current-buffer))
         (win (selected-window))
         (new-win nil)
         (frm nil))
    (when (string= (buffer-name buf) mu4e~view-buffer-name)
      ;; rename it so that it will not be found by mu4e-view
      (rename-buffer (concat "*mu4e-view*"
                             (mu4e-msg-field mu4e~view-msg :subject) "*") t)
      (setq mu4e~view-buffer nil)
      (if toframe
          (progn
            (setq frm (make-frame))
            (select-window win))
        (setq new-win (split-window-below)))
      (mu4e-view mu4e~view-msg mu4e~view-headers-buffer)
      (when focusnew
        (if toframe
            (select-frame frm)
          (select-window new-win))))))
