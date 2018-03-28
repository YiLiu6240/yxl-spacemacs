(setq yxl-git-packages '(magit
                         git-gutter
                         git-timemachine
                         magit-org-todos))

(defun yxl-git/post-init-magit ()
  (with-eval-after-load 'magit
    ;; magit speedup:
    ;; stop magit from generating diffs when doing commits, slow
    ;; use C-c C-d to show diff again
    ;; (remove-hook 'server-switch-hook 'magit-commit-diff)
    (add-hook 'magit-mode-hook #'visual-line-mode)
    (magit-add-section-hook
     'magit-status-sections-hook
     'magit-insert-standup-commits
     'magit-insert-staged-changes
     t)
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

(defun yxl-git/post-init-git-gutter ()
  (with-eval-after-load 'git-gutter
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

(defun yxl-git/post-init-git-timemachine ()
  (with-eval-after-load 'git-timemachine
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

(defun yxl-git/init-magit-org-todos ()
  (use-package magit-org-todos
    :after magit
    :init
    (progn
      (setq magit-org-todos-filename "TODO.org"))
    :config
    (progn
      (magit-add-section-hook
       'magit-status-sections-hook
       'magit-org-todos-insert-org-todos
       'magit-insert-staged-changes
       t))))
