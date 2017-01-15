(use-package yxl-utils
  :load-path "~/local-repo/yxl-emacs-goodies/yxl-utils")

(use-package yxl-evil
  :load-path "~/local-repo/yxl-emacs-goodies/yxl-evil"
  :after evil)

(use-package yxl-helm
  :after 'helm
  :load-path "~/local-repo/yxl-emacs-goodies/yxl-helm"
  :commands (yxl-helm-hotspot
             yxl-helm-org-files
             yxl-helm-websites
             yxl-helm-files)
  :defer t)

(use-package yxl-ess
  :load-path "~/local-repo/yxl-emacs-goodies/yxl-ess"
  :after ess-site)

(use-package yxl-org-patch
  :load-path "~/local-repo/yxl-emacs-goodies/yxl-org-patch"
  :after (org))

(use-package yxl-elfeed
  :load-path "~/local-repo/yxl-emacs-goodies/yxl-elfeed"
  :defer t
  :after (helm elfeed)
  :config
  (progn
    (yxl-elfeed-patch)))
