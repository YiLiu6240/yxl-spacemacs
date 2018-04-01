;; HACK: somehow my own helm config layer is not respected
;; overwrite spacemacs default
(with-eval-after-load 'helm
  ;; rebind C-z to C-tab, easier to press
  (define-key helm-map (kbd "C-<tab>") 'helm-select-action)
  (define-key helm-map (kbd "C-z") nil)
  (define-key helm-map (kbd "C-h") 'backward-delete-char))

(with-eval-after-load 'helm-files
  ;; overwrite spacemacs default
  (define-key helm-find-files-map (kbd "C-h") 'backward-delete-char)
  (define-key helm-read-file-map (kbd "C-h") 'backward-delete-char))

;; override spacemacs popwin
(delete '("^\*WoMan.+\*$" :regexp t :position bottom)
      popwin:special-display-config)
;; (delete '("*Help*" :dedicated t :position bottom :stick t :noselect t :height 0.4)
;;       popwin:special-display-config)

;; HACK maining ivy compatibility
(setq helm-enable-auto-resize nil)
(setq helm-use-fuzzy t)
