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
