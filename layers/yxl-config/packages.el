(setq yxl-config-packages
      `(pdf-tools
        python
        imenu-list
        hippie-exp
        projectile
        ,(when (configuration-layer/layer-usedp 'git)
           'magit)
        ibuffer
        neotree
        graphviz
        deft
        ,(when (configuration-layer/layer-usedp 'version-control)
           `git-gutter)
        ,(when (configuration-layer/layer-usedp 'git)
           'git-timemachine)))

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
    (setq my-todo-occur-regex
          "\\<\\(FIXME\\|TODO\\|BUG\\|ISSUE\\|DOING\\|NEXT\\)")
    (add-to-list 'projectile-globally-ignored-file-suffixes ".html")
    (add-to-list 'projectile-globally-ignored-files "*.html")
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
    ;; (remove-hook 'server-switch-hook 'magit-commit-diff)
    (add-hook 'magit-mode-hook #'visual-line-mode)
    (setq vc-handled-backends nil)
    (setq auto-revert-buffer-list-filter
          'magit-auto-revert-repository-buffers-p)
    (setq magit-refresh-status-buffer nil)
    ;; other configs
    (setq magit-log-arguments '("-n15" "--graph" "--decorate" "--follow"))
    ;; prefer two way ediff
    (setq magit-ediff-dwim-show-on-hunks t)
    ;; word diff
    (setq magit-diff-refine-hunk 'all)
    ;; log
    (setq magit-log-margin '(t "%Y-%m-%d %H:%M %a " magit-log-margin-width t 18))
    ;; bindings
    (evil-define-key 'normal magit-mode-map (kbd "C-S-j") #'magit-section-forward)
    (evil-define-key 'normal magit-mode-map (kbd "C-S-k") #'magit-section-backward)
    (evil-define-key 'normal magit-mode-map (kbd "C-h") #'windmove-left)
    (evil-define-key 'normal magit-mode-map (kbd "C-j") #'windmove-down)
    (evil-define-key 'normal magit-mode-map (kbd "C-k") #'windmove-up)
    (evil-define-key 'normal magit-mode-map (kbd "C-l") #'windmove-right)
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

;; TODO: refine this with vc-layer
(defun yxl-config/post-init-git-gutter ()
  (with-eval-after-load 'git-gutter
    (defun git-gutter-reshape (gutter)
      "Re-shape gutter for `ivy-read'.
Source: http://blog.binchen.org/posts/enhance-emacs-git-gutter-with-ivy-mode.html"
      (let* ((linenum-start (aref gutter 3))
             (linenum-end (aref gutter 4))
             (target-line "")
             (target-linenum 1)
             (tmp-line "")
             (max-line-length 0))
        (save-excursion
          (while (<= linenum-start linenum-end)
            (goto-line linenum-start)
            (setq tmp-line (replace-regexp-in-string
                            "^[ \t]*" ""
                            (buffer-substring (line-beginning-position)
                                              (line-end-position))))
            (when (> (length tmp-line) max-line-length)
              (setq target-linenum linenum-start)
              (setq target-line tmp-line)
              (setq max-line-length (length tmp-line)))

            (setq linenum-start (1+ linenum-start))))
        ;; build (key . linenum-start)
        (cons (format "%s %d: %s"
                      (if (eq 'deleted (aref gutter 1)) "-" "+")
                      target-linenum target-line)
              target-linenum)))
    (defun git-gutter-ivy-select ()
      "Source: http://blog.binchen.org/posts/enhance-emacs-git-gutter-with-ivy-mode.html"
      (interactive)
      (if git-gutter:diffinfos
          (ivy-read "git-gutters:"
                    (mapcar 'git-gutter-reshape git-gutter:diffinfos)
                    :action (lambda (e)
                              ;; ivy9+ keep `(car e)'
                              ;; ivy8- strip the `(car e)'
                              ;; we handle both data structure
                              (unless (numberp e) (setq e (cdr e)))
                              (goto-line e)))
        (message "NO git-gutters!")))
    (spacemacs|define-transient-state vcs
      :title "VCS Transient State"
      :doc "
 Hunk Commands^^^^^^                 Magit Commands
----------------------------^^^^^^  ------------------------------------------
 [_n_]^^^^      next hunk            [_w_/_u_]^^    stage/unstage in current file
 [_N_/_p_]^^    previous hunk        [_c_/_C_]^^    commit with popup/direct commit
 [_r_/_s_/_h_]  revert/stage/show    [_f_/_F_/_P_]  fetch/pull/push popup
 [_t_]^^^^      toggle diff signs    [_l_/_D_]^^    log/diff popup
 [_v_] ivy-select"
      :on-enter (spacemacs/vcs-enable-margin)
      :bindings
      ("C" magit-commit :exit t)
      ("d" magit-ediff-popup :exit t)
      ("D" magit-diff-unstaged :exit t)
      ("F" magit-pull-popup :exit t)
      ("P" magit-push-popup :exit t)
      ("c" magit-commit-popup :exit t)
      ("f" magit-fetch-popup :exit t)
      ("l" magit-log-popup :exit t)
      ("u" magit-unstage-file)
      ("w" magit-stage-file)
      ("n" spacemacs/vcs-next-hunk)
      ("N" spacemacs/vcs-previous-hunk)
      ("p" spacemacs/vcs-previous-hunk)
      ("r" spacemacs/vcs-revert-hunk)
      ("s" spacemacs/vcs-stage-hunk)
      ("h" spacemacs/vcs-show-hunk)
      ("t" spacemacs/toggle-version-control-margin)
      ("v" git-gutter-ivy-select)
      ("q" nil :exit t))))

(defun yxl-config/post-init-git-timemachine ()
  (with-eval-after-load 'git-timemachine
    (defun git-timemachine-show-selected-revision ()
      "Show last (current) revision of file.
Source: http://blog.binchen.org/posts/new-git-timemachine-ui-based-on-ivy-mode.html"
      (interactive)
      (let* ((collection (mapcar (lambda (rev)
                                   ;; re-shape list for the ivy-read
                                   (cons (concat (substring-no-properties (nth 0 rev) 0 7) "|" (nth 5 rev) "|" (nth 6 rev)) rev))
                                 (git-timemachine--revisions))))
        (ivy-read "commits:"
                  collection
                  :action (lambda (rev)
                            ;; compatible with ivy 9+ and ivy 8
                            (unless (string-match-p "^[a-z0-9]*$" (car rev))
                              (setq rev (cdr rev)))
                            (git-timemachine-show-revision rev)))))
    (defun git-timemachine-ivy-select ()
      "Open git snapshot with the selected version.  Based on ivy-mode.
Source: http://blog.binchen.org/posts/new-git-timemachine-ui-based-on-ivy-mode.html"
      (interactive)
      (git-timemachine--start #'git-timemachine-show-selected-revision))
    (spacemacs|define-transient-state time-machine
      :title "Git Timemachine Transient State"
      :doc "
[_t_] ivy-select
[_p_/_N_] previous [_n_] next [_c_] current [_g_] goto nth rev [_Y_] copy hash [_q_] quit"
        :on-enter (let (golden-ratio-mode)
                    (unless (bound-and-true-p git-timemachine-mode)
                      (call-interactively 'git-timemachine)))
        :on-exit (when (bound-and-true-p git-timemachine-mode)
                   (git-timemachine-quit))
        :foreign-keys run
        :bindings
        ("t" git-timemachine-ivy-select)
        ("c" git-timemachine-show-current-revision)
        ("g" git-timemachine-show-nth-revision)
        ("p" git-timemachine-show-previous-revision)
        ("n" git-timemachine-show-next-revision)
        ("N" git-timemachine-show-previous-revision)
        ("Y" git-timemachine-kill-revision)
        ("q" nil :exit t))))
