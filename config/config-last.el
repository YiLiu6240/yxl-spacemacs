(advice-add 'spacemacs-buffer/goto-buffer
            :override
            (lambda (&optional refresh)
              (interactive)
              (find-file "~/Downloads/")))

;; NOTE: Frustrated with org's task system
;; (org-agenda nil "A")
(require 'org)
;; HACK:
;;   Speed up loading of org-capture
;;
;;   something is wrong with several packages
;;   loaded despite of use-package
;;   when org-capture is called the first time.
;;
;;   Need to review use-package following
;;   Spacemacs' dev update
(call-interactively 'org-store-link)
