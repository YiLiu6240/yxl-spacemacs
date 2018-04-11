(defun yxl-base/home ()
  "Force recreation of the spacemacs buffer.

Accepts C-u arg to delete all other windows."
  (interactive)
  (when (equal current-prefix-arg '(4))
    (delete-other-windows))
  (setq spacemacs-buffer--last-width nil)
  (spacemacs-buffer/goto-buffer t))

(defun yxl-base//setup-evil-hack ()
  "Personal opinionated hacks of evil bindings."
  (with-eval-after-load 'evil
    (dolist (map `(,evil-normal-state-map
                   ,evil-motion-state-map
                   ,evil-evilified-state-map-original))
      (define-key map (kbd "C-,") #'yxl-hydra-hotspot/body)
      (define-key map (kbd "C-.") #'yxl-hydra-system/body)
      (define-key map (kbd "C-;") #'yxl-find-file-counsel)
      (define-key map (kbd "C-:") #'ibuffer)
      (define-key map (kbd "C-/") #'treemacs-toggle))))
