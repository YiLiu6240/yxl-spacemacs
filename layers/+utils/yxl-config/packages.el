(setq yxl-config-packages '(pdf-tools
                            python
                            imenu-list
                            hippie-exp
                            projectile
                            ibuffer
                            neotree
                            graphviz
                            deft))

(defun yxl-config/post-init-pdf-tools ()
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

(defun yxl-config/post-init-python ()
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

(defun yxl-config/post-init-imenu-list ()
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

(defun yxl-config/post-init-hippie-exp ()
  (define-key evil-insert-state-map (kbd "C-p") #'previous-line))

(defun yxl-config/post-init-projectile ()
  (with-eval-after-load 'projectile
    ;; inherit from zilongshanren
    (evil-set-initial-state 'occur-mode 'evilified)
    (add-to-list 'projectile-globally-ignored-file-suffixes ".html")
    (add-to-list 'projectile-globally-ignored-files "*.html")
    (defun my/todo-occur ()
      (interactive)
      (if (projectile-project-p)
          (multi-occur (projectile-project-buffers) hl-todo-regexp)
        (occur my-todo-occur-regex)))
    (spacemacs/declare-prefix "p/" "TODO-occur")
    (spacemacs/set-leader-keys "p/t" #'my/todo-occur)))

(defun yxl-config/post-init-ibuffer ()
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
    (yxl-config/setup-ibuffer-bindings)))

(defun yxl-config/post-init-neotree ()
  (defun yxl-neotree-enter-external ()
    "Open with a program from a list of registered programs."
    (interactive)
    (neo-buffer--execute nil 'yxl-neo-open-file-external 'neo-open-dir))
  (defun yxl-neo-open-file-external (full-path arg)
    "Open with a program from a list of registered programs."
    (yxl-open-file-external full-path))
  (with-eval-after-load 'neotree
    (define-key neotree-mode-map "o" #'spacemacs/neotree-expand-or-open)
    (define-key neotree-mode-map "O" #'neotree-enter-ace-window)
    (define-key neotree-mode-map "x" #'yxl-neotree-enter-external)))

(defun yxl-config/post-init-graphviz ()
  (with-eval-after-load 'graphviz-dot-mode
    (define-key graphviz-dot-mode-map ";" nil)))

(defun yxl-config/post-init-deft ()
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
