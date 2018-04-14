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
      ;; C - ,: always refer to the local-leader s s, which defaults to
      ;;        eval in repl (specific to major mode)
      (define-key map (kbd "C-,")
        (kbd (concat dotspacemacs-major-mode-leader-key " ss")))
      ;; C - .: personal hydra; C - S - .: system hydra
      (define-key map (kbd "C-.") #'yxl-hydra-hotspot/body)
      (define-key map (kbd "C->") #'yxl-hydra-system/body)
      ;; C - ;: find file; C - S - ;: ibuffer
      (define-key map (kbd "C-;") #'yxl-find-file-counsel)
      (define-key map (kbd "C-:") #'ibuffer)
      (define-key map (kbd "C-/") #'treemacs-toggle))))
