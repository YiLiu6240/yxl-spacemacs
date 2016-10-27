(setq yxl-evil-packages '(evil
                          evil-surround
                          evil-indent-plus
                          evil-textobj-column
                          (evil-little-word :location local)
                          evil-mc
                          evil-evilified-state))
;; evil-cleverparens

(defun yxl-evil/post-init-evil ()
  (with-eval-after-load 'evil
    (require 'evil-goodies)
    (yxl-evil/setup-evil)
    (yxl-evil/setup-evil-misc)))

(defun yxl-evil/post-init-evil-evilified-state ()
  (with-eval-after-load 'evil-evilified-state
    (yxl-evil/setup-evilified)))

(defun yxl-evil/post-init-evil-surround ()
  (with-eval-after-load 'evil-surround
    (add-hook 'prog-mode-hook #'yxl-evil/evil-surround-pairs)
    (add-hook 'text-mode-hook #'yxl-evil/evil-surround-pairs)))

(defun yxl-evil/post-init-evil-indent-plus ()
  (with-eval-after-load 'evil-indent-plus
    (defun evil-indent-plus--higher-indent-range (&optional point)
      "Return the point at the begin and end of the text block with greater indentation.
If `point' is supplied and non-nil it will return the begin and end of the block surrounding point."
      (save-excursion
        (when point
          (goto-char point))
        (let ((base (current-indentation))
              (begin (point))
              (end (point)))
          (setq begin (point-at-bol))
          (setq end (evil-indent-plus--seek end 1 t t #'evil-indent-plus--g-or-empty-p))
          (message "begin (%s) end (%s)" begin end)
          (list begin end base))))

    (evil-define-text-object evil-indent-plus-c-indent (&optional count beg end type)
      "Text object describing the block with the same (or greater) indentation as the current line,
skipping empty lines."
      :type line
      (evil-indent-plus--linify (evil-indent-plus--higher-indent-range)))

    (defun yxl-evil/go-up-indent ()
      (interactive)
      (let ((base (current-indentation))
            (begin (point)))
        (setq begin (evil-indent-plus--seek begin -1 nil t #'evil-indent-plus--geq-or-empty-p))
        (goto-char begin)))

    (define-key evil-motion-state-map "gh" #'yxl-evil/go-up-indent)
    (define-key evil-inner-text-objects-map "q" #'evil-indent-plus-c-indent)))

(defun yxl-evil/init-evil-textobj-column ()
  (use-package evil-textobj-column
    :init
    (define-key evil-inner-text-objects-map "k" #'evil-textobj-column-word)
    (define-key evil-inner-text-objects-map "K" #'evil-textobj-column-WORD)))

(defun yxl-evil/init-evil-little-word ()
  "from theBB's github"
  (use-package evil-little-word
    :commands (evil-forward-little-word-begin
               evil-backward-little-word-begin
               evil-forward-little-word-end
               evil-backward-little-word-end
               evil-a-little-word
               evil-inner-little-word)
    :init
    (progn
      (define-key evil-motion-state-map (kbd "glw") #'evil-forward-little-word-begin)
      (define-key evil-motion-state-map (kbd "glb") #'evil-backward-little-word-begin)
      (define-key evil-motion-state-map (kbd "glW") #'evil-forward-little-word-end)
      (define-key evil-motion-state-map (kbd "glB") #'evil-backward-little-word-end)
      (define-key evil-outer-text-objects-map (kbd "lw") #'evil-a-little-word)
      (define-key evil-inner-text-objects-map (kbd "lw") #'evil-inner-little-word))))

(defun yxl-evil/post-init-evil-mc ()
  ;; https://github.com/TheBB/spacemacs-layers/blob/master/init.el
  (add-hook 'prog-mode-hook #'turn-on-evil-mc-mode)
  (add-hook 'text-mode-hook #'turn-on-evil-mc-mode)
  (add-hook 'evil-mc-after-cursors-deleted
            (defun bb/clear-anzu ()
              (interactive)
              (setq anzu--state nil))))

;; (defun yxl-evil/init-evil-cleverparens ()
;;   (use-package evil-cleverparens
;;     :defer t
;;     :diminish evil-cleverparens-mode
;;     :init
;;     (progn
;;       (setq evil-cleverparens-use-regular-insert t)
;;       (spacemacs|add-toggle evil-cleverparens
;;         :status evil-cleverparens-mode
;;         :on  (evil-cleverparens-mode)
;;         :off (evil-cleverparens-mode -1)
;;         :documentation "Enable evil-cleverparens."))))
