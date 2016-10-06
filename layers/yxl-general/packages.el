(setq yxl-general-packages '(dired
                             pdf-tools
                             calfw
                             ess
                             python
                             comint
                             ibuffer
                             imenu-list
                             company
                             hippie-exp
                             projectile
                             magit
                             ibuffer
                             neotree))

(defun yxl-general/post-init-dired ()
  (with-eval-after-load 'dired
    (evilified-state-evilify dired-mode dired-mode-map
      "o"  #'dired-find-file
      "O"  #'dired-find-file-other-window
      "q"  #'evil-quit
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
      "gt" #'eyebrowse-next-window-config
      ;; from vinegar layer
      "0"         'dired-back-to-start-of-files
      "="         'vinegar/dired-diff
      "I"         'vinegar/dotfiles-toggle
      (kbd "~")   '(lambda ()(interactive) (find-alternate-file "~/"))
      "T"         'dired-tree-down
      "f"         (if (configuration-layer/layer-usedp 'ivy)
                      'counsel-find-file
                    'helm-find-files)
      "J"         'dired-goto-file
      (kbd "C-f") 'find-name-dired
      "K"         'dired-do-kill-lines
      "r"         'revert-buffer
      (kbd "C-r") 'dired-do-redisplay)))

(defun yxl-general/post-init-pdf-tools ()
  (with-eval-after-load 'pdf-tools

    (setq-default pdf-view-midnight-colors '("#839496" . "#15262c"))
    (add-hook 'pdf-view-mode-hook #'pdf-view-midnight-minor-mode)

    ;; bug workaround wrt eyebrowse
    ;; https://github.com/politza/pdf-tools/issues/225
    (defun window-state-put-workaround (&rest _args)
      (run-with-idle-timer 0 nil #'run-window-configuration-change-hook))
    (advice-add 'window-state-put :after #'window-state-put-workaround)

    (evil-set-initial-state 'pdf-view-mode 'evilified)
    (evilified-state-evilify pdf-tools pdf-view-mode-map
      "-" #'dired-jump
      "_" #'pdf-view-shrink
      "+" #'pdf-view-enlarge
      "gg" #'yxl/pdf-view-goto-first-page
      "G" #'yxl/pdf-view-goto-page
      "e" #'pdf-view-scroll-down-or-previous-page
      "d" #'pdf-view-scroll-up-or-next-page
      "j"  #'pdf-view-next-line-or-next-page
      "k"  #'pdf-view-previous-line-or-previous-page
      "n" #'pdf-view-next-page
      "p" #'pdf-view-previous-page
      "J" #'pdf-view-next-page
      "K" #'pdf-view-previous-page
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
      "d" #'pdf-view-midday-minor-mode)
    (spacemacs|define-transient-state pdf-tools
        :title "PDF-tools transient state"
        :on-enter (setq which-key-inhibit t)
        :on-exit (setq which-key-inhibit nil)
        :evil-leader-for-mode (pdf-view-mode . ".")
        :doc
        "
 Navigation^^^^                Scale/Fit^^                    Annotations^^       Actions^^           Other^^
 ----------^^^^--------------- ---------^^------------------  -----------^^------ -------^^---------- -----^^---
 [_j_/_k_] scroll down/up      [_W_] fit to width             [_al_] list         [_s_] search         [_=_] enlarge
 [_h_/_l_] scroll left/right   [_H_] fit to height            [_at_] text         [_O_] outline        [_-_] shrink
 [_d_/_u_] pg down/up          [_P_] fit to page              [_aD_] delete       [_p_] print          [_q_] quit
 [_J_/_K_] next/prev pg        [_m_] slice using mouse        [_am_] markup       [_o_] open link
 [_0_/_$_] full scroll l/r     [_b_] slice from bounding box  ^^                  [_r_] revert
 ^^^^                          [_R_] reset slice              ^^                  [_t_] attachments
 ^^^^                          [_zr_] reset zoom              ^^                  [_n_] night mode
 "
        :bindings
        ;; Navigation
        ("j"  pdf-view-next-line-or-next-page)
        ("k"  pdf-view-previous-line-or-previous-page)
        ("l"  image-forward-hscroll)
        ("h"  image-backward-hscroll)
        ("J"  pdf-view-next-page)
        ("K"  pdf-view-previous-page)
        ("u"  pdf-view-scroll-down-or-previous-page)
        ("d"  pdf-view-scroll-up-or-next-page)
        ("0"  image-bol)
        ("$"  image-eol)
        ;; zoom
        ("-" pdf-view-shrink)
        ("=" pdf-view-enlarge)
        ;; Scale/Fit
        ("W"  pdf-view-fit-width-to-window)
        ("H"  pdf-view-fit-height-to-window)
        ("P"  pdf-view-fit-page-to-window)
        ("m"  pdf-view-set-slice-using-mouse)
        ("b"  pdf-view-set-slice-from-bounding-box)
        ("R"  pdf-view-reset-slice)
        ("zr" pdf-view-scale-reset)
        ;; Annotations
        ("aD" pdf-annot-delete)
        ("at" pdf-annot-attachment-dired :exit t)
        ("al" pdf-annot-list-annotations :exit t)
        ("am" pdf-annot-add-markup-annotation)
        ;; Actions
        ("s" pdf-occur :exit t)
        ("O" pdf-outline :exit t)
        ("p" pdf-misc-print-document :exit t)
        ("o" pdf-links-action-perform :exit t)
        ("r" pdf-view-revert-buffer)
        ("t" pdf-annot-attachment-dired :exit t)
        ("n" pdf-view-midnight-minor-mode)
        ;; Other
        ("q" nil :exit t))))

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
    ;; stop magit from generating diffs when doing commits, slow
    (remove-hook 'server-switch-hook 'magit-commit-diff)
    (setq vc-handled-backends nil)
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
