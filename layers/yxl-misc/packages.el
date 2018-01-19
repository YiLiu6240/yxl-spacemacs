(setq yxl-misc-packages '(nov))

(defun yxl-misc/init-nov ()
  (use-package nov
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))
    :config
    (progn
      (add-hook 'nov-mode-hook
                (lambda ()
                  (setq line-spacing 4)
                  (setq visual-fill-column-center-text t)
                  (setq visual-fill-column-width 120)
                  (visual-fill-column-mode t)
                  (visual-line-mode t)))
      (evilified-state-evilify-map nov-mode-map
        :mode nov-mode
        :bindings
        "r" #'nov-render-document
        "J" #'nov-next-document
        "K" #'nov-previous-document
        "m" #'nov-display-metadata
        "V" #'nov-view-content-source
        "v" #'nov-view-source
        "t" #'nov-goto-toc)
      (spacemacs/set-leader-keys-for-major-mode 'nov-mode
        "g" #'nov-render-document
        "r" #'nov-render-document
        "J" #'nov-next-document
        "K" #'nov-previous-document
        "m" #'nov-display-metadata
        "V" #'nov-view-content-source
        "v" #'nov-view-source
        "t" #'nov-goto-toc))))
