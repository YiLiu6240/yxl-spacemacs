(setq yxl-mail-packages '(gnus
                          mu4e
                          bbdb
                          counsel-bbdb))

(defun yxl-mail/post-init-mu4e ()
 (with-eval-after-load 'mu4e
   ;; TODO: change this
   (setq mu4e-account-alist
      '(("gmail"
         ;; Under each account, set the account-specific variables you want.
         (mu4e-sent-messages-behavior delete)
         (mu4e-sent-folder "/gmail/[Gmail]/.Sent Mail")
         (mu4e-drafts-folder "/gmail/[Gmail]/.Drafts")
         (user-mail-address "billy@gmail.com")
         (user-full-name "Billy"))
        ("college"
         (mu4e-sent-messages-behavior sent)
         (mu4e-sent-folder "/college/Sent Items")
         (mu4e-drafts-folder "/college/Drafts")
         (user-mail-address "bb15@college.edu")
         (user-full-name "Billy Bob 15"))))
   (mu4e/mail-account-reset)
   ;;; Set up some common mu4e variables
   (setq mu4e-maildir "~/.mail"
         mu4e-trash-folder "/Trash"
         mu4e-refile-folder "/Archive"
         mu4e-get-mail-command "mbsync -a"
         mu4e-update-interval nil
         mu4e-compose-signature-auto-include nil
         mu4e-view-show-images t
         mu4e-view-show-addresses t)
   ;;; Mail directory shortcuts
   (setq mu4e-maildir-shortcuts
         '(("/gmail/INBOX" . ?g)))
   ;;; Bookmarks
   (setq mu4e-bookmarks
         `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
           ("date:today..now" "Today's messages" ?t)
           ("date:7d..now" "Last 7 days" ?w)
           ("mime:image/*" "Messages with images" ?p)
           (,(mapconcat 'identity
                        (mapcar
                         (lambda (maildir)
                           (concat "maildir:" (car maildir)))
                         mu4e-maildir-shortcuts) " OR ")
            "All inboxes" ?i)))))

(defun yxl-mail/post-init-gnus ()
  ;; following the example from CHEN Bin
  ;; separate config file at
  ;; ~/.gnus.el
  (with-eval-after-load 'gnus
    (require 'nnir)
    ;; ask encryption password once
    (setq epa-file-cache-passphrase-for-symmetric-encryption t)
    (setq gnus-thread-sort-functions
          '(gnus-thread-sort-by-most-recent-date
            (not gnus-thread-sort-by-number)))
    ;; NO 'passive
    (setq gnus-use-cache t)
    ;; Fetch only part of the article if we can.
    ;; I saw this in someone's .gnus
    (setq gnus-read-active-file 'some)
    ;; open attachment
    (with-eval-after-load 'mailcap
      (cond
       ;; on OSX, maybe change mailcap-mime-data?
       ((eq system-type 'darwin))
       ;; on Windows, maybe change mailcap-mime-data?
       ((eq system-type 'windows-nt))
       (t
        ;; Linux, read ~/.mailcap
        (mailcap-parse-mailcaps))))
    ;; Threads!  I hate reading un-threaded email -- especially mailing
    ;; lists.  This helps a ton!
    (setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)
    ;; Also, I prefer to see only the top level message.  If a message has
    ;; several replies or is part of a thread, only show the first message.
    ;; `gnus-thread-ignore-subject' will ignore the subject and
    ;; look at 'In-Reply-To:' and 'References:' headers.
    (setq gnus-thread-hide-subtree t)
    (setq gnus-thread-ignore-subject t)
    ;; Read HTML mail
    ;; You need install the command line web browser 'w3m' and Emacs plugin 'w3m'
    (setq mm-text-html-renderer 'w3m)
    ;; http://www.gnu.org/software/emacs/manual/html_node/gnus/_005b9_002e2_005d.html
    (setq gnus-use-correct-string-widths nil)
    (yxl-mail/setup-gnus-bindings)))

(defun yxl-mail/init-bbdb ()
  (use-package bbdb
    :defer t))

(defun yxl-mail/init-counsel-bbdb ()
  (use-package consel-bbdb
    :defer t
    :config
    (progn
      (setq counsel-bbdb-customized-insert
            (lambda (r append-comma)
              (let* ((family-name (nth 1 r))
                     (given-name (nth 2 r))
                     (display-name (nth 3 r))
                     (mail (nth 4 r))))
              (insert (format "%s:%s:%s:%s%s"
                              given-name
                              family-name
                              display-name
                              mail
                              (if append-comma ", " " "))))))))
