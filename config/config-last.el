(advice-add 'spacemacs-buffer/goto-buffer
            :override
            (lambda (&optional refresh)
              (interactive)
              (find-file "~/Downloads/")))

;; Simply load the most frequently used org file
(find-file yxl-base-org-today)
