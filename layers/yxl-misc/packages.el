(setq yxl-misc-packages '(nov))

(defun yxl-misc/init-nov ()
  (use-package nov
    :defer t
    :config
    (progn
      (add-hook 'nov-mode-hook
                (lambda ()
                  (setq line-spacing 4)
                  (setq visual-fill-column-center-text t)
                  (setq visual-fill-column-width 120)
                  (visual-fill-column-mode t)
                  (visual-line-mode t)))
      (define-key nov-mode-map "." #'nov-hydra/body)
      (define-key nov-mode-map "J" #'nov-next-document)
      (define-key nov-mode-map "K" #'nov-previous-document)
      (defhydra nov-hydra (:color blue :hint nil :columns 4
                                  :pre (setq which-key-inhibit t)
                                  :post (setq which-key-inhibit nil))
        ("g" nov-render-document "nov-render-document")
        ("J" nov-next-document "nov-next-document")
        ("K" nov-previous-document "nov-previous-document")
        ("m" nov-display-metadata "nov-display-metadata")
        ("V" nov-view-content-source "nov-view-content-source")
        ("v" nov-view-source "nov-view-source")
        ("t" nov-goto-toc "nov-goto-toc")))))
