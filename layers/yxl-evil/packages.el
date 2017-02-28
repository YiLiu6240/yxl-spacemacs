(setq yxl-evil-packages '(evil
                          evil-evilified-state
                          (yxl-evil :location site)
                          evil-multiedit
                          evil-surround
                          evil-textobj-column))
                          ;; evil-mc

(defun yxl-evil/post-init-evil ()
  (with-eval-after-load 'evil
    (yxl-evil/setup-evil-main)
    (yxl-evil/setup-evil-personal)
    (yxl-evil/setup-evil-misc)
    ;; TODO: check if this causes trouble
    (setq evil-move-beyond-eol t)))

(defun yxl-evil/post-init-evil-evilified-state ()
  (with-eval-after-load 'evil-evilified-state
    (yxl-evil/setup-evilified)
    (yxl-evil/setup-evilified-personal)))

(defun yxl-evil/init-yxl-evil ()
  (use-package yxl-evil
    :after evil))

(defun yxl-evil/init-evil-multiedit ()
  (use-package evil-multiedit
    :after evil
    :config
    (progn
      ;; Highlights all matches of the selection in the buffer.
      (define-key evil-visual-state-map "R" 'evil-multiedit-match-all)
      ;; Match the word under cursor (i.e. make it an edit region). Consecutive presses will
      ;; incrementally add the next unmatched match.
      (define-key evil-normal-state-map (kbd "M-d") 'evil-multiedit-match-and-next)
      ;; Match selected region.
      (define-key evil-visual-state-map (kbd "M-d") 'evil-multiedit-match-and-next)
      ;; Same as M-d but in reverse.
      (define-key evil-normal-state-map (kbd "M-D") 'evil-multiedit-match-and-prev)
      (define-key evil-visual-state-map (kbd "M-D") 'evil-multiedit-match-and-prev)
      ;; OPTIONAL: If you prefer to grab symbols rather than words, use
      ;; `evil-multiedit-match-symbol-and-next` (or prev).
      ;; Restore the last group of multiedit regions.
      (define-key evil-visual-state-map (kbd "C-M-D") 'evil-multiedit-restore)
      ;; RET will toggle the region under the cursor
      (define-key evil-multiedit-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)
      ;; ...and in visual mode, RET will disable all fields outside the selected region
      (define-key evil-motion-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)
      ;; For moving between edit regions
      (define-key evil-multiedit-state-map (kbd "C-n") 'evil-multiedit-next)
      (define-key evil-multiedit-state-map (kbd "C-p") 'evil-multiedit-prev)
      (define-key evil-multiedit-insert-state-map (kbd "C-n") 'evil-multiedit-next)
      (define-key evil-multiedit-insert-state-map (kbd "C-p") 'evil-multiedit-prev)
      ;; Ex command that allows you to invoke evil-multiedit with a regular expression, e.g.
      (evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match))))

(defun yxl-evil/post-init-evil-surround ()
  (with-eval-after-load 'evil-surround
    (add-hook 'prog-mode-hook #'yxl-evil/evil-surround-pairs)
    (add-hook 'text-mode-hook #'yxl-evil/evil-surround-pairs)))

(defun yxl-evil/init-evil-textobj-column ()
  (use-package evil-textobj-column))

;; (defun yxl-evil/post-init-evil-mc ()
;;   ;; https://github.com/TheBB/spacemacs-layers/blob/master/init.el
;;   (add-hook 'prog-mode-hook #'turn-on-evil-mc-mode)
;;   (add-hook 'text-mode-hook #'turn-on-evil-mc-mode)
;;   (add-hook 'evil-mc-after-cursors-deleted
;;             (defun bb/clear-anzu ()
;;               (interactive)
;;               (setq anzu--state nil))))
