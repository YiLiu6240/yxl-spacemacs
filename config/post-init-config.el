(with-eval-after-load 'helm
  ;; rebind C-z to C-tab, easier to press
  (define-key helm-map (kbd "C-<tab>") 'helm-select-action)
  (define-key helm-map (kbd "C-z") nil)
  (define-key helm-map (kbd "C-h") 'backward-delete-char))

(with-eval-after-load 'helm-files
  ;; overwrite spacemacs default
  (define-key helm-find-files-map (kbd "C-h") 'backward-delete-char)
  (define-key helm-read-file-map (kbd "C-h") 'backward-delete-char))

;; ivy views
(with-eval-after-load 'ivy
  (setq ivy-use-virtual-buffers nil)
  (load (concat dotspacemacs-directory "config/yxl-ivy-views.el"))
  )
