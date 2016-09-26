(setq yxl-general-packages '(dired
                             pdf-tools
                             calfw
                             ess
                             python
                             comint
                             ibuffer
                             imenu-list
                             eyebrowse
                             company
                             hippie-exp
                             projectile
                             magit
                             ibuffer
                             neotree
                             elfeed))

(defun yxl-general/post-init-dired ()
  (with-eval-after-load 'dired
    (evilified-state-evilify dired-mode dired-mode-map
      "o"  #'dired-find-file
      "O"  #'dired-find-file-other-window
      "q"  #'evil-quit
      ;; "u"  #'dired-up-directory
      ;; "M"  #'dired-unmark
      ;; PATCH: vinegar:
      ;; fix issue causing all window housing the same dired buffer to jump
      "-"  #'dired-up-directory
      "gg" #'evil-goto-first-line
      "G"  #'evil-goto-line
      (kbd "C-h") #'windmove-left
      (kbd "C-j") #'windmove-down
      (kbd "C-k") #'windmove-up
      (kbd "C-l") #'windmove-right
      "H" #'eyebrowse-prev-window-config
      "L" #'eyebrowse-next-window-config
      "gT" #'eyebrowse-prev-window-config
      "gt" #'eyebrowse-next-window-config)))

(defun yxl-general/post-init-pdf-tools ()
  (with-eval-after-load 'pdf-tools

    (setq-default pdf-view-midnight-colors '("#839496" . "#132126"))
    (add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode)

    ;; bug workaround wrt eyebrowse
    ;; https://github.com/politza/pdf-tools/issues/225
    (defun window-state-put-workaround (&rest _args)
      (run-with-idle-timer 0 nil #'run-window-configuration-change-hook))
    (advice-add 'window-state-put :after #'window-state-put-workaround)

    (evil-set-initial-state 'pdf-view-mode 'evilified)
    (evilified-state-evilify pdf-tools pdf-view-mode-map
      "-" #'dired-jump
      "_" #'projectile-dired
      "gg" #'yxl/pdf-view-goto-first-page
      "G" #'yxl/pdf-view-goto-page
      "e" #'pdf-view-scroll-down-or-previous-page
      "d" #'pdf-view-scroll-up-or-next-page
      "j"  #'pdf-view-next-line-or-next-page
      "k"  #'pdf-view-previous-line-or-previous-page
      "l"  #'image-forward-hscroll
      "h"  #'image-backward-hscroll
      (kbd "C-h") #'windmove-left
      (kbd "C-j") #'windmove-down
      (kbd "C-k") #'windmove-up
      (kbd "C-l") #'windmove-right
      "H" #'eyebrowse-prev-window-config
      "L" #'eyebrowse-next-window-config)
    (spacemacs/set-leader-keys-for-major-mode #'pdf-view-mode
      "=" #'pdf-view-enlarge
      "-" #'pdf-view-shrink
      "gg" #'yxl/pdf-view-goto-first-page
      "G" #'yxl/pdf-view-goto-page
      "os" #'yxl/helm-pdf-occur
      "oS" #'yxl/pdf-occur-search-preset
      "n" #'pdf-view-midnight-minor-mode
      "N" #'pdf-view-darknight-minor-mode
      "d" #'pdf-view-midday-minor-mode)))

(defun yxl-general/post-init-calfw ()
  (with-eval-after-load 'calfw
    (evilified-state-evilify-map cfw:calendar-mode-map
      :mode cfw:calendar-mode
      :bindings
      (kbd "C-h") #'windmove-left
      (kbd "C-j") #'windmove-down
      (kbd "C-k") #'windmove-up
      (kbd "C-l") #'windmove-right
      "H" #'eyebrowse-prev-window-config
      "L" #'eyebrowse-next-window-config)))

(defun yxl-general/post-init-ess ()
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
      (evil-set-initial-state 'ess-rdired-mode 'evilified)
      (evil-define-key 'evilified ess-rdired-mode-map
        (kbd "C-h") #'windmove-left
        (kbd "C-j") #'windmove-down
        (kbd "C-k") #'windmove-up
        (kbd "C-l") #'windmove-right
        "H" #'eyebrowse-prev-window-config
        "L" #'eyebrowse-next-window-config))
    (with-eval-after-load 'ess-help
      (evil-set-initial-state 'ess-help-mode 'evilified)
      (evilified-state-evilify-map ess-help-mode-map
        :mode ess-help-mode
        :bindings
        (kbd "C-h") #'windmove-left
        (kbd "C-j") #'windmove-down
        (kbd "C-k") #'windmove-up
        (kbd "C-l") #'windmove-right
        "H" #'eyebrowse-prev-window-config
        "L" #'eyebrowse-next-window-config))))

(defun yxl-general/post-init-python ()
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

(defun yxl-general/post-init-ibuffer ()
  (progn
    (evilified-state-evilify ibuffer-mode ibuffer-mode-map
      "o" #'ibuffer-visit-buffer
      "O" #'ibuffer-visit-buffer-other-window)))

(defun yxl-general/post-init-imenu-list ()
  (progn
    (setq imenu-list-auto-resize nil)
    (setq imenu-list-size 0.15)
    (defun imenu-list-select-window ()
      (interactive)
      (if (eq (get-buffer-window imenu-list-buffer-name)
              nil)
          (imenu-list-minor-mode))
      (select-window (get-buffer-window imenu-list-buffer-name)))
    (spacemacs/set-leader-keys "bI" #'imenu-list-select-window)
    (evilified-state-evilify imenu-list imenu-list-major-mode-map
      (kbd "C-h") #'windmove-left
      (kbd "C-j") #'windmove-down
      (kbd "C-k") #'windmove-up
      (kbd "C-l") #'windmove-right
      "H" #'eyebrowse-prev-window-config
      "L" #'eyebrowse-next-window-config)))

(defun yxl-general/post-init-eyebrowse ()
  (with-eval-after-load 'eyebrowse

    (add-to-list 'window-persistent-parameters '(window-side . writable))
    (add-to-list 'window-persistent-parameters '(window-slot . writable))
    ;; let workspace inherit previous state
    ;; (setq eyebrowse-new-workspace nil)
    (spacemacs|define-micro-state workspaces
      :doc (spacemacs//workspaces-ms-documentation)
      :use-minibuffer t
      :bindings
      ("0" eyebrowse-switch-to-window-config-0)
      ("1" eyebrowse-switch-to-window-config-1)
      ("2" eyebrowse-switch-to-window-config-2)
      ("3" eyebrowse-switch-to-window-config-3)
      ("4" eyebrowse-switch-to-window-config-4)
      ("5" eyebrowse-switch-to-window-config-5)
      ("6" eyebrowse-switch-to-window-config-6)
      ("7" eyebrowse-switch-to-window-config-7)
      ("8" eyebrowse-switch-to-window-config-8)
      ("9" eyebrowse-switch-to-window-config-9)
      ("<tab>" eyebrowse-last-window-config)
      ("C-i" eyebrowse-last-window-config)
      ("C" eyebrowse-close-window-config)
      ("H" eyebrowse-prev-window-config)
      ("L" eyebrowse-next-window-config)
      ;; ("n" eyebrowse-next-window-config)
      ;; ("N" eyebrowse-prev-window-config)
      ;; ("p" eyebrowse-prev-window-config)
      ("r" spacemacs/workspaces-ms-rename :exit t)
      ("w" eyebrowse-switch-to-window-config :exit t))))

(defun yxl-general/post-init-company ()
  (with-eval-after-load 'company
    ;; (define-key company-active-map (kbd "<ESC>") #'company-cancel)
    (define-key company-active-map (kbd "C-h") nil)
    (define-key company-active-map (kbd "C-j") #'company-select-next)
    (define-key company-active-map (kbd "C-k") #'company-select-previous)
    (define-key company-active-map (kbd "C-l") nil)))

(defun yxl-general/post-init-hippie-exp ()
  (define-key evil-insert-state-map (kbd "C-p") #'previous-line))

(defun yxl-general/post-init-projectile ()
  (with-eval-after-load 'projectile
    ;; inherit from zilongshanren
    (evil-set-initial-state 'occur-mode 'evilified)
    (setq my-todo-occur-regex
          "\\<\\(FIXME\\|TODO\\|BUG\\|ISSUE\\|DOING\\|NEXT\\):")
    (defun my/todo-occur ()
      (interactive)
      (if (projectile-project-p)
          (multi-occur (projectile-project-buffers) my-todo-occur-regex)
        (occur my-todo-occur-regex)))
    (spacemacs/declare-prefix "p/" "TODO-occur")
    (spacemacs/set-leader-keys "p/t" #'my/todo-occur)))

(defun yxl-general/post-init-magit ()
  (with-eval-after-load 'magit

    (define-key magit-status-mode-map (kbd "C-M-1") #'magit-jump-to-unstaged)
    (define-key magit-status-mode-map (kbd "C-M-2") #'magit-jump-to-untracked)
    (define-key magit-status-mode-map (kbd "C-M-3") #'magit-jump-to-staged)
    (define-key magit-status-mode-map (kbd "C-M-4") #'magit-jump-to-stashes))

  ;; prefer two way ediff
  (setq magit-ediff-dwim-show-on-hunks t))

(defun yxl-general/post-init-ibuffer ()
  (with-eval-after-load 'ibuffer
    (setq ibuffer-formats
          '((mark modified read-only " "
                  (name 30 30 :left :elide) " "
                  (size 9 -1 :right) " "
                  (mode 16 16 :left :elide) " " filename-and-process)
            (mark " " (name 16 -1) " " filename)))))

(defun yxl-general/post-init-neotree ()
  (with-eval-after-load 'neotree
    (define-key neotree-mode-map "o" #'spacemacs/neotree-expand-or-open)))

(defun yxl-general/post-init-elfeed ()
  (with-eval-after-load 'elfeed

    ;; disable images fetching by default
    (add-hook 'elfeed-search-mode-hook (lambda ()
                                         (setq shr-inhibit-images t)))
    (add-hook 'elfeed-show-mode-hook (lambda ()
                                         (setq shr-inhibit-images t)))

    (unbind-key "b" elfeed-search-mode-map)
    (unbind-key "b" elfeed-show-mode-map)

    (evilified-state-evilify-map elfeed-search-mode-map
      :mode elfeed-search-mode
      :eval-after-load elfeed-search
      :bindings
      ",c"  #'elfeed-db-compact
      ",gr" #'elfeed-update
      ",gR" #'elfeed-search-update--force
      ",gu" #'elfeed-unjam
      "c" nil
      "gr" nil
      "gR" nil
      "gu" nil
      "o" #'elfeed-search-show-entry
      "O" #'elfeed-search-browse-url
      ",R" #'zilong/elfeed-mark-all-as-read
      "m" #'yxl/elfeed-mark-as-read
      "M" #'yxl/elfeed-mark-as-unread
      ",tm" #'elfeed-toggle-shr-inhibit-images
      "q"  #'quit-window
      (kbd "C-h") #'windmove-left
      (kbd "C-j") #'windmove-down
      (kbd "C-k") #'windmove-up
      (kbd "C-l") #'windmove-right
      "H" #'eyebrowse-prev-window-config
      "L" #'eyebrowse-next-window-config
      "w"  nil
      "W"  nil)
    (evilified-state-evilify-map elfeed-show-mode-map
      :mode elfeed-show-mode
      :eval-after-load elfeed-show
      :bindings
      "q" #'quit-window
      ",tm" #'elfeed-toggle-shr-inhibit-images
      ",O" #'elfeed-search-browse-url
      (kbd "C-h") #'windmove-left
      (kbd "C-j") #'windmove-down
      (kbd "C-k") #'windmove-up
      (kbd "C-l") #'windmove-right
      "H" #'eyebrowse-prev-window-config
      "L" #'eyebrowse-next-window-config
      (kbd "C-n") #'elfeed-show-next
      (kbd "C-p") #'elfeed-show-prev)

    ;; this is for test purposes
    (spacemacs/set-leader-keys-for-major-mode 'elfeed-search-mode
      "q" #'quit-window)

    (defadvice elfeed-show-yank (after elfeed-show-yank-to-kill-ring activate compile)
      "Insert the yanked text from x-selection to kill ring"
      (kill-new (x-get-selection)))

    (ad-activate 'elfeed-show-yank)))
