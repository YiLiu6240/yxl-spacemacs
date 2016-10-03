(setq yxl-workspace-packages '(eyebrowse))

(defun yxl-workspace/post-init-eyebrowse ()
  (with-eval-after-load 'eyebrowse

    (add-to-list 'window-persistent-parameters '(window-side . writable))
    (add-to-list 'window-persistent-parameters '(window-slot . writable))
    ;; let workspace inherit previous state
    ;; (setq eyebrowse-new-workspace nil)
    (spacemacs|define-micro-state workspaces
      :doc (spacemacs//workspaces-ms-documentation)
      :use-minibuffer t
      :bindings
      ("0" eyebrowse-switch-to-window-config-0)
      ("1" eyebrowse-switch-to-window-config-1)
      ("2" eyebrowse-switch-to-window-config-2)
      ("3" eyebrowse-switch-to-window-config-3)
      ("4" eyebrowse-switch-to-window-config-4)
      ("5" eyebrowse-switch-to-window-config-5)
      ("6" eyebrowse-switch-to-window-config-6)
      ("7" eyebrowse-switch-to-window-config-7)
      ("8" eyebrowse-switch-to-window-config-8)
      ("9" eyebrowse-switch-to-window-config-9)
      ("<tab>" eyebrowse-last-window-config)
      ("C-i" eyebrowse-last-window-config)
      ("C" eyebrowse-close-window-config)
      ("H" eyebrowse-prev-window-config)
      ("L" eyebrowse-next-window-config)
      ;; ("n" eyebrowse-next-window-config)
      ;; ("N" eyebrowse-prev-window-config)
      ;; ("p" eyebrowse-prev-window-config)
      ("r" spacemacs/workspaces-ms-rename :exit t)
      ("w" eyebrowse-switch-to-window-config :exit t))))
