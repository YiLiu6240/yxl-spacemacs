(setq yxl-config-packages '(dired
                            pdf-tools
                            ess
                            python
                            imenu-list
                            company
                            hippie-exp
                            projectile
                            magit
                            ibuffer
                            neotree
                            graphviz))

(defun yxl-config/post-init-dired ()
  (with-eval-after-load 'dired
    (evilified-state-evilify dired-mode dired-mode-map
      "o"  #'dired-find-file
      "O"  #'dired-find-file-other-window
      "q"  #'yxl-dired-delete-window
      "-"  #'dired-up-directory
      ;; from vinegar layer
      "0"         'dired-back-to-start-of-files
      "="         'vinegar/dired-diff
      "I"         'vinegar/dotfiles-toggle
      ;; do not kill previous buffer, useful in splits
      (kbd "~")   '(lambda ()(interactive) (find-file "~/"))
      "T"         'dired-tree-down
      "f"         (if (configuration-layer/layer-usedp 'ivy)
                      'counsel-find-file
                    'helm-find-files)
      "J"         'dired-goto-file
      (kbd "C-f") 'find-name-dired
      "K"         'dired-do-kill-lines
      "r"         'revert-buffer
      (kbd "C-r") 'dired-do-redisplay)))

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

(defun yxl-config/post-init-ess ()
  (with-eval-after-load 'ess-mode
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
    (with-eval-after-load 'ess-mode
      (evil-set-initial-state 'ess-rdired-mode 'evilified))
    (with-eval-after-load 'ess-help
      (evil-set-initial-state 'ess-help-mode 'evilified))))

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

(defun yxl-config/post-init-company ()
  (with-eval-after-load 'company
    ;; (define-key company-active-map (kbd "<ESC>") #'company-cancel)
    (define-key company-active-map (kbd "C-h") nil)
    (define-key company-active-map (kbd "C-j") #'company-select-next)
    (define-key company-active-map (kbd "C-k") #'company-select-previous)
    (define-key company-active-map (kbd "C-l") nil)))

(defun yxl-config/post-init-hippie-exp ()
  (define-key evil-insert-state-map (kbd "C-p") #'previous-line))

(defun yxl-config/post-init-projectile ()
  (with-eval-after-load 'projectile
    ;; inherit from zilongshanren
    (evil-set-initial-state 'occur-mode 'evilified)
    (setq my-todo-occur-regex
          "\\<\\(FIXME\\|TODO\\|BUG\\|ISSUE\\|DOING\\|NEXT\\)")
    (defun my/todo-occur ()
      (interactive)
      (if (projectile-project-p)
          (multi-occur (projectile-project-buffers) my-todo-occur-regex)
        (occur my-todo-occur-regex)))
    (spacemacs/declare-prefix "p/" "TODO-occur")
    (spacemacs/set-leader-keys "p/t" #'my/todo-occur)))

(defun yxl-config/post-init-magit ()
  (with-eval-after-load 'magit
    ;; magit speedup:
    ;; stop magit from generating diffs when doing commits, slow
    ;; use C-c C-d to show diff again
    (remove-hook 'server-switch-hook 'magit-commit-diff)
    (add-hook 'magit-mode-hook #'visual-line-mode)
    (setq vc-handled-backends nil)
    (setq magit-log-arguments '("-n15" "--graph" "--decorate"))
    ;; prefer two way ediff
    (setq magit-ediff-dwim-show-on-hunks t)
    ;; word diff
    (setq magit-diff-refine-hunk 'all)
    ;; bindings
    (evil-define-key 'normal magit-mode-map (kbd "C-S-j") #'magit-section-forward)
    (evil-define-key 'normal magit-mode-map (kbd "C-S-k") #'magit-section-backward)
    (evil-define-key 'normal magit-mode-map (kbd "C-h") #'windmove-left)
    (evil-define-key 'normal magit-mode-map (kbd "C-j") #'windmove-down)
    (evil-define-key 'normal magit-mode-map (kbd "C-k") #'windmove-up)
    (evil-define-key 'normal magit-mode-map (kbd "C-l") #'windmove-right)
    (evil-define-key 'normal magit-mode-map "H" #'eyebrowse-prev-window-config)
    (evil-define-key 'normal magit-mode-map "L" #'eyebrowse-next-window-config)
    (define-key magit-status-mode-map (kbd "C-M-1") #'magit-jump-to-unstaged)
    (define-key magit-status-mode-map (kbd "C-M-2") #'magit-jump-to-untracked)
    (define-key magit-status-mode-map (kbd "C-M-3") #'magit-jump-to-staged)
    (define-key magit-status-mode-map (kbd "C-M-4") #'magit-jump-to-stashes)))

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
  (with-eval-after-load 'neotree
    (define-key neotree-mode-map "o" #'spacemacs/neotree-expand-or-open)))

(defun yxl-config/post-init-graphviz ()
  (with-eval-after-load 'graphviz-dot-mode
    (define-key graphviz-dot-mode-map ";" nil)))
