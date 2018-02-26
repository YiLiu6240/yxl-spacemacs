(advice-add 'spacemacs-buffer/goto-buffer
            :override
            (lambda (&optional refresh)
              (interactive)
              (find-file "~/Downloads/")))

(org-agenda nil "A")
