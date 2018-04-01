(defun yxl-dired/dired-sidebar-keybinding-override ()
  "Override evilified bindings (inherited from dired) in dired-sidebar."
  (define-key evil-evilified-state-local-map
    (kbd "-") #'dired-sidebar-up-directory))
