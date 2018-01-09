;; (org-agenda nil "A")

(advice-add 'spacemacs-buffer/goto-buffer
            :override
            (lambda (&optional refresh)
              (interactive)
              (find-file "~/Downloads/")))
