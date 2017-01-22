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


;; add align actions
;; https://github.com/syl20bnr/spacemacs/pull/8118/files
(spacemacs|create-align-repeat-x "left-curly-brace" "{")
(spacemacs|create-align-repeat-x "right-curly-brace" "}" t)
(spacemacs|create-align-repeat-x "left-square-brace" "\\[")
(spacemacs|create-align-repeat-x "right-square-brace" "\\]" t)
