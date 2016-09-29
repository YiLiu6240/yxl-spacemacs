(setq yxl-evil-packages '(evil
                          evil-indent-plus
                          evil-textobj-column
                          evil-visual-mark-mode
                          (evil-little-word :location local)
                          evil-mc
                          evil-cleverparens))

(defun yxl-evil/post-init-evil ()
  (with-eval-after-load 'evil

    ;; the evil way is to use "fd"
    (define-key evil-insert-state-map (kbd "C-h") #'backward-delete-char-untabify)
    (define-key evil-insert-state-map (kbd "C-d") #'delete-forward-char)
    (define-key evil-insert-state-map (kbd "C-a") #'beginning-of-line-text)
    (define-key evil-insert-state-map (kbd "C-e") #'end-of-line)
    (define-key evil-insert-state-map (kbd "C-p") #'previous-line)
    (define-key evil-insert-state-map (kbd "C-n") #'next-line)

    ;; (define-key evil-motion-state-map "j" #'evil-next-visual-line)
    ;; (define-key evil-motion-state-map "k" #'evil-previous-visual-line)
    (define-key evil-motion-state-map "j" #'evil-next-line)
    (define-key evil-motion-state-map "k" #'evil-previous-line)
    ;; Also in visual mode
    ;; (define-key evil-visual-state-map "j" #'evil-next-visual-line)
    ;; (define-key evil-visual-state-map "k" #'evil-previous-visual-line)
    ;; force evil-jump-forward
    (define-key evil-motion-state-map (kbd "C-i") #'evil-jump-forward)
    ;; "g" related commands --------
    ;; mark: repalce with evil-middle-of-visual-line
    (define-key evil-motion-state-map "gm" #'evil-goto-mark)
    (define-key evil-motion-state-map "gt" #'eyebrowse-next-window-config)
    (define-key evil-motion-state-map "gT" #'eyebrowse-prev-window-config)
    (define-key evil-motion-state-map "gH" #'evil-first-non-blank)
    (define-key evil-motion-state-map "gL" #'evil-end-of-line)
    ;; --------
    ;; "tab" navigation
    (define-key evil-motion-state-map "H" #'eyebrowse-prev-window-config)
    (define-key evil-motion-state-map "L" #'eyebrowse-next-window-config)
    (define-key evil-motion-state-map (kbd "C-h") #'windmove-left)
    (define-key evil-motion-state-map (kbd "C-j") #'windmove-down)
    (define-key evil-motion-state-map (kbd "C-k") #'windmove-up)
    (define-key evil-motion-state-map (kbd "C-l") #'windmove-right)
    ;; misc
    (define-key evil-motion-state-map (kbd "C-S-p") #'helm-M-x)
    (define-key evil-normal-state-map (kbd "_") #'projectile-dired)
    (define-key evil-normal-state-map "q" nil)
    (define-key evil-normal-state-map "qm" #'evil-execute-macro)
    (define-key evil-normal-state-map "qM" #'evil-record-macro)
    (define-key evil-normal-state-map "qq" #'evil-quit)
    (define-key evil-normal-state-map "qQ" #'evil-save-and-close)
    (define-key evil-normal-state-map "qw" #'evil-write)
    (define-key evil-normal-state-map "qW" #'evil-write-all)
    ;; TODO: ex commands for tabe and tabnew -- write functions for eyebrowse
    ;; ---- disabled ----
    ;; vim-surround, use "S"
    ;; (define-key 'visual evil-surround-mode-map "s" #'evil-substitute)
    ;; (define-key 'visual evil-surround-mode-map "S" #'evil-surround-region)
    ;; ;; swap colon and semi colon
    ;; (define-key evil-normal-state-map "g:" #'goto-last-change)
    ;; ;; (define-key evil-motion-state-map ":" #'evil-repeat-find-char)
    )

  ;; Define history commands for comint
  (with-eval-after-load 'evil
    (evil-define-key 'insert comint-mode-map
      (kbd "C-j") #'windmove-down
      (kbd "C-k") #'windmove-up
      (kbd "C-p") #'comint-previous-input
      (kbd "C-n") #'comint-next-input)
    (evil-define-key 'normal comint-mode-map
      (kbd "C-j") #'windmove-down
      (kbd "C-k") #'windmove-up
      (kbd "C-p") #'comint-previous-input
      (kbd "C-n") #'comint-next-input)))

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
          ;; (setq begin (evil-indent-plus--seek begin -1 t nil #'evil-indent-plus--geq-or-empty-p))
          (setq begin (point-at-bol))
          (setq end (evil-indent-plus--seek end 1 t t #'evil-indent-plus--g-or-empty-p))
          (message "begin (%s) end (%s)" begin end)
          (list begin end base))))

    (evil-define-text-object evil-indent-plus-c-indent (&optional count beg end type)
      "Text object describing the block with the same (or greater) indentation as the current line,
skipping empty lines."
      :type line
      (evil-indent-plus--linify (evil-indent-plus--higher-indent-range)))

    (define-key evil-inner-text-objects-map "q" #'evil-indent-plus-c-indent)))

(defun yxl-evil/init-evil-textobj-column ()
  (use-package evil-textobj-column
    :init
    (define-key evil-inner-text-objects-map "k" #'evil-textobj-column-word)
    (define-key evil-inner-text-objects-map "K" #'evil-textobj-column-WORD)))

(defun yxl-evil/init-evil-visual-mark-mode ()
  (use-package evil-visual-mark-mode
    :defer t
    :init
    (spacemacs|add-toggle evil-visual-mark-mode
      :status evil-visual-mark-mode
      :on (evil-visual-mark-mode)
      :off (evil-visual-mark-mode -1)
      :documentation "Enable evil visual marks mode."
      :evil-leader "t`")))

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

(defun yxl-evil/init-evil-cleverparens ()
  (use-package evil-cleverparens
    :defer t
    :diminish evil-cleverparens-mode
    :init
    (progn
      (setq evil-cleverparens-use-regular-insert t)
      (spacemacs|add-toggle evil-cleverparens
        :status evil-cleverparens-mode
        :on  (evil-cleverparens-mode)
        :off (evil-cleverparens-mode -1)
        :documentation "Enable evil-cleverparens."))))
