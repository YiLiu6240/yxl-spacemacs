(defun yxl-mail/setup-gnus-bindings ()
  (evilified-state-evilify gnus-group-mode gnus-group-mode-map
    (kbd "o") #'gnus-topic-select-group)
  (with-eval-after-load 'gnus-group
    (defhydra hydra-gnus-group (:color blue)
      "Do?"
      ("a" gnus-group-list-active "REMOTE groups A A")
      ("l" gnus-group-list-all-groups "LOCAL groups L")
      ("c" gnus-topic-catchup-articles "Read all c")
      ("G" gnus-group-make-nnir-group "Search server G G")
      ("g" gnus-group-get-new-news "Refresh g")
      ("s" gnus-group-enter-server-mode "Servers")
      ;; ("m" gnus-group-new-mail "Compose m OR C-x m")
      ("m" compose-mail "Compose m OR C-x m")
      ("#" gnus-topic-mark-topic "mark #")
      ("q" nil "cancel"))
    (define-key gnus-group-mode-map "." 'hydra-gnus-group/body))

  ;; gnus-summary-mode
  (evilified-state-evilify gnus-summary-mode gnus-summary-mode-map
    (kbd "J") #'gnus-summary-next-article
    (kbd "K") #'gnus-summary-prev-article
    (kbd "<RET>") #'spacemacs/browse-nnrss-url
    (kbd "o") #'spacemacs/browse-nnrss-url)
  (with-eval-after-load 'gnus-sum
    (defhydra hydra-gnus-summary (:color blue)
      "Do?"
      ("s" gnus-summary-show-thread "Show thread")
      ("h" gnus-summary-hide-thread "Hide thread")
      ("n" gnus-summary-insert-new-articles "Refresh / N")
      ("f" gnus-summary-mail-forward "Forward C-c C-f")
      ("!" gnus-summary-tick-article-forward "Mail -> disk !")
      ("p" gnus-summary-put-mark-as-read "Mail <- disk")
      ("c" gnus-summary-catchup-and-exit "Read all c")
      ("e" gnus-summary-resend-message-edit "Resend S D e")
      ("R" gnus-summary-reply-with-original "Reply with original R")
      ("r" gnus-summary-reply "Reply r")
      ("W" gnus-summary-wide-reply-with-original "Reply all with original S W")
      ("w" gnus-summary-wide-reply "Reply all S w")
      ("#" gnus-topic-mark-topic "mark #")
      ("q" nil "cancel"))
    (define-key gnus-summary-mode-map "." 'hydra-gnus-summary/body))

  ;; gnus-article-mode
  (with-eval-after-load 'gnus-art
    (defhydra hydra-gnus-article (:color blue)
      "Do?"
      ("f" gnus-summary-mail-forward "Forward")
      ("R" gnus-article-reply-with-original "Reply with original R")
      ("r" gnus-article-reply "Reply r")
      ("W" gnus-article-wide-reply-with-original "Reply all with original S W")
      ("o" gnus-mime-save-part "Save attachment at point o")
      ("w" gnus-article-wide-reply "Reply all S w")
      ("q" nil "cancel"))
    (define-key gnus-article-mode-map "." 'hydra-gnus-article/body))

  (with-eval-after-load 'message
    (defhydra hydra-message (:color blue)
      "Do?"
      ("ca" mml-attach-file "Attach C-c C-a")
      ("cc" message-send-and-exit "Send C-c C-c")
      ("q" nil "cancel"))
    (global-set-key (kbd "C-c C-y") 'hydra-message/body)))
