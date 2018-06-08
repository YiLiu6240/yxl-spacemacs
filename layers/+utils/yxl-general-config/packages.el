(setq yxl-general-config-packages '(pdf-tools
                                    python
                                    imenu-list
                                    hippie-exp
                                    ibuffer
                                    graphviz
                                    deft))

(defun yxl-general-config/post-init-pdf-tools ()
  (with-eval-after-load 'pdf-tools

    (pdf-view-refresh-midnight-colors)

    (add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode)
    (add-hook 'yxl-switch-theme-hook #'pdf-view-refresh-midnight-colors)
    ;; bug workaround wrt eyebrowse
    ;; https://github.com/politza/pdf-tools/issues/225
    (defun window-state-put-workaround (&rest _args)
      (run-with-idle-timer 0 nil #'run-window-configuration-change-hook))
    (advice-add 'window-state-put :after #'window-state-put-workaround)
    (yxl-pdf-view-bindings)))

(defun yxl-general-config/post-init-python ()
  (with-eval-after-load 'python
    (evil-define-key 'insert comint-mode-map
      (kbd "C-j") #'windmove-down
      (kbd "C-k") #'windmove-up
      (kbd "C-p") #'comint-previous-input
      (kbd "C-n") #'comint-next-input)
    (evil-define-key 'normal comint-mode-map
      (kbd "C-j") #'windmove-down
      (kbd "C-k") #'windmove-up
      (kbd "C-p") #'comint-previous-input
      (kbd "C-n") #'comint-next-input)
    (evil-define-key 'normal inferior-python-mode-map
      (kbd "C-h") #'windmove-left
      (kbd "C-j") #'windmove-down
      (kbd "C-k") #'windmove-up
      (kbd "C-l") #'windmove-right
      (kbd "C-p") #'comint-previous-input
      (kbd "C-n") #'comint-next-input)))

(defun yxl-general-config/post-init-imenu-list ()
  (progn
    (setq imenu-list-auto-resize nil)
    (setq imenu-list-size 0.15)
    (defun imenu-list-select-window ()
      (interactive)
      (if (eq (get-buffer-window imenu-list-buffer-name)
              nil)
          (imenu-list-minor-mode))
      (select-window (get-buffer-window imenu-list-buffer-name)))
    (spacemacs/set-leader-keys "bI" #'imenu-list-select-window)))

(defun yxl-general-config/post-init-hippie-exp ()
  (define-key evil-insert-state-map (kbd "C-p") #'previous-line))

(defun yxl-general-config/post-init-ibuffer ()
  (with-eval-after-load 'ibuffer
    (evilified-state-evilify ibuffer-mode ibuffer-mode-map
      "o" #'ibuffer-visit-buffer
      "O" #'ibuffer-visit-buffer-other-window)
    (setq ibuffer-formats
          '((mark modified read-only " "
                  (name 30 30 :left :elide) " "
                  (size 9 -1 :right) " "
                  (mode 16 16 :left :elide) " " filename-and-process)
            (mark " " (name 16 -1) " " filename)))
    (yxl-general-config/setup-ibuffer-bindings)))

(defun yxl-general-config/post-init-graphviz ()
  (with-eval-after-load 'graphviz-dot-mode
    (define-key graphviz-dot-mode-map ";" nil)))

(defun yxl-general-config/post-init-deft ()
  (with-eval-after-load 'deft
    (progn
      (evil-define-key 'insert deft-mode-map (kbd "C-h") #'deft-filter-decrement)
      (defun deft-open-file-at-point (&optional arg)
        "Interactive version of `def-open-file'."
        (interactive)
        (let ((file (deft-filename-at-point)))
          (when file
            (deft-open-file file nil arg))))
      (spacemacs/set-leader-keys-for-major-mode 'deft-mode
        "d" #'deft-delete-file
        "i" #'deft-toggle-incremental-search
        "n" #'deft-new-file
        "o" #'deft-open-file-at-point
        "O" #'deft-open-file-other-window
        "r" #'deft-rename-file))))
