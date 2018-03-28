(defun yxl-datascience/ess-setup-keybindings ()
  ;; move "<-" key to "C-c ="
  (setq ess-S-assign "<-")
  (setq ess-smart-S-assign-key (kbd "C-c ="))
  (ess-toggle-S-assign nil)
  (ess-toggle-S-assign nil)

  (define-key ess-mode-map (kbd "C-c C-.") #'yxl-datascience/insert-pipe)
  (define-key ess-mode-map (kbd "C-<tab>") #'sp-indent-adjust-sexp)
  (define-key ess-mode-map (kbd "C-S-<tab>") #'sp-dedent-adjust-sexp)
  (define-key ess-mode-map
    (kbd "M--") (lambda () (interactive)
                  (if (equal (string (preceding-char)) " ")
                      (insert "<- ")
                    (insert " <- "))))
  (define-key inferior-ess-mode-map
    (kbd "M--") (lambda () (interactive)
                  (if (equal (string (preceding-char)) " ")
                      (insert "<- ")
                    (insert " <- "))))
  (define-key ess-mode-map
    (kbd "C-S-M") (lambda () (interactive)
                    (if (equal (string (preceding-char)) " ")
                        (insert "%>% ")
                      (insert " %>% "))))
  (define-key inferior-ess-mode-map
    (kbd "C-S-M") (lambda () (interactive)
                    (if (equal (string (preceding-char)) " ")
                        (insert "%>% ")
                      (insert " %>% "))))
  (evil-define-key 'normal inferior-ess-mode-map
    (kbd "C-d") #'evil-scroll-down)
  (define-key ess-mode-map (kbd "C-,") #'ess-eval-region-or-line-and-step)
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


(defun yxl-datascience/ess-set-leader-keys-for-mode (mode)
  (spacemacs/set-leader-keys-for-major-mode mode
    ";" #'ess-execute
    "a" #'yxl-ess-atpoint
    "A" #'yxl-ess-atpoint-pop
    "of" #'yxl-ess-call-useful-funcs
    "os" #'yxl-ess-call-atpoint-str
    "oS" #'yxl-ess-call-atpoint-generic
    "R" #'yxl-ess-open-rstudio
    "d" #'ess-rdired
    "i" #'yxl-ess-repl-popup
    ;; send
    "sa" #'ess-switch-process
    "st" nil
    "sT" nil
    "sf" #'ess-eval-function
    "sF" #'ess-eval-function-and-go
    "sp" #'ess-eval-paragraph-and-step
    "sP" #'ess-eval-pipe-through-line
    "ss" #'ess-switch-to-ESS
    "sU" #'ess-install-library
    "su" #'ess-load-library
    "sw" #'ess-set-working-directory
    "s:" #'ess-execute
    ;; R helpers
    "hh" #'ess-help
    "hH" #'ess-display-help-on-object
    "ha" #'ess-display-help-apropos
    "hb" #'ess-display-help-in-browser
    "hi" #'ess-R-object-popup
    "hp" #'ess-display-package-index
    "hv" #'ess-display-vignettes
    "hw" #'ess-help-web-search
    ;; Developer bindings
    "DT" 'ess-build-tags-for-directory
    "Ds" 'ess-set-style
    "Dg" 'ess-dump-object-into-edit-buffer
    ;; TODO only show these bindings if we're in R-mode.
    "Dl" 'ess-r-devtools-load-package
    "Dp" 'ess-r-devtools-set-pacakge
    "Dt" 'ess-r-devtools-test-pacakge
    "Dc" 'ess-r-devtools-check-pacakge
    "Dr" 'ess-r-devtools-document-package
    "Du" 'ess-r-devtools-unload-package
    "Di" 'ess-r-devtools-install-package
    ;; debug bindings
    "bT" 'ess-show-traceback
    ;; "b~" 'ess-show-callstack
    ;; "bC" 'ess-show-callstack
    "bs" 'ess-bp-set
    "be" 'ess-debug-toggle-error-action
    "bc" 'ess-bp-set-conditional
    "bl" 'ess-bp-set-logger
    "bt" 'ess-bp-toggle-state
    "bd" 'ess-bp-kill
    "bD" 'ess-bp-kill-all
    "bn" 'ess-bp-next
    "bp" 'ess-bp-previous
    "bm" 'ess-debug-flag-for-debugging
    "bM" 'ess-debug-unflag-for-debugging
    "bw" 'ess-watch
    ;; roxygen
    "rh" 'ess-roxy-hide-all
    "rr" 'ess-roxy-update-entry
    "rn" 'ess-roxy-next-entry
    "rp" 'ess-roxy-previous-entry
    "rP" 'ess-roxy-preview-text
    "rt" 'ess-roxy-toggle-hiding
    ;; other views
    "vi" #'asb-ess-R-object-popup-str
    "vI" #'asb-ess-R-object-popup-interactive
    "vp" #'ess-R-dv-pprint
    "vt" #'ess-R-dv-ctable))

(defun yxl-datascience/ess-declare-prefix-for-mode (mode)
  (spacemacs/declare-prefix-for-mode mode "ma" "atpoint")
  (spacemacs/declare-prefix-for-mode mode "ms" "repl-interaction")
  (spacemacs/declare-prefix-for-mode mode "mh" "help")
  (spacemacs/declare-prefix-for-mode mode "mD" "developer")
  (spacemacs/declare-prefix-for-mode mode "mb" "debugging")
  (spacemacs/declare-prefix-for-mode mode "mv" "views")
  (spacemacs/declare-prefix-for-mode mode "mr" "roxygen")
  (spacemacs/declare-prefix-for-mode mode "mo" "user-defined"))

(defun yxl-datascience/setup-julia-keybindings ()
  (spacemacs/set-leader-keys-for-major-mode 'ess-julia-mode
    "<tab>" #'julia-latexsub-or-indent))

(defun yxl-datascience/setup-jupyter-keybindings ()
  (with-eval-after-load 'ein-notebooklist
    (define-key ein:notebooklist-mode-map
      "o" 'spacemacs/ace-buffer-links))
  (define-key ein:notebook-mode-map
    (kbd "C-c C-h") #'yxl-datascience/jupyter-help)
  (evil-define-key 'insert ein:notebook-multilang-mode-map
    (kbd "<C-return>") #'ein:worksheet-execute-cell
    (kbd "<S-return>") #'ein:worksheet-execute-cell-and-goto-next)
  (evil-define-key 'normal ein:notebook-multilang-mode-map
    ;; keybindings mirror ipython web interface behavior
    (kbd "<C-return>") #'ein:worksheet-execute-cell
    (kbd "<S-return>") #'ein:worksheet-execute-cell-and-goto-next
    "gj" #'ein:worksheet-goto-next-input
    "gk" #'ein:worksheet-goto-prev-input)
  (define-key ein:notebook-multilang-mode-map
    (kbd "M-j") #'ein:worksheet-move-cell-down)
  (define-key ein:notebook-multilang-mode-map
    (kbd "M-k") #'ein:worksheet-move-cell-up)
  (define-key ein:edit-cell-mode-map
    (kbd "<C-return>") #'ein:edit-cell-save-and-execute-and-exit))

(defun yxl-datascience/setup-jupyter-evilified-keybindings ()
  (evilified-state-evilify-map ein:pager-mode-map
    :mode ein:pager-mode
    :eval-after-load ein-pager
    :bindings
    (kbd "q") #'quit-window)
  (evilified-state-evilify-map ein:notebooklist-mode-map
    :mode ein:notebooklist-mode
    :eval-after-load ein-notebooklist
    :bindings
    (kbd "O") #'spacemacs/ace-buffer-links))

(defun yxl-datascience/setup-jupyter-leader-keys ()
  (spacemacs/declare-prefix-for-mode 'ein:notebook-multilang-mode
    "mw" "worksheet")
  (spacemacs/declare-prefix-for-mode 'ein:notebook-multilang-mode
    "mc" "cell")
  (spacemacs/declare-prefix-for-mode 'ein:notebook-multilang-mode
    "mn" "notebook")
  (spacemacs/set-leader-keys-for-major-mode 'ein:notebook-multilang-mode
    "." #'yxl-datascience/jupyter-hydra/body
    dotspacemacs-major-mode-leader-key
    #'ein:worksheet-execute-cell-and-goto-next
    "o" #'ein:worksheet-insert-cell-below
    "O" #'ein:worksheet-insert-cell-above
    "C-o" #'ein:worksheet-insert-cell-below-and-edit
    ;; cell-wise
    "cc" #'ein:edit-cell-contents
    "cd" #'ein:worksheet-kill-cell
    "cp" #'ein:worksheet-yank-cell
    "cu" #'ein:worksheet-change-cell-type
    "ct" #'ein:worksheet-toggle-output
    "cS" #'ein:worksheet-toggle-slide-type
    "c C-l" #'ein:worksheet-clear-output
    "c C-k" #'ein:worksheet-merge-cell
    "c C-j" #'spacemacs/ein:worksheet-merge-cell-next
    ;; notebook-wise
    "nx" #'ein:notebook-close
    "cy" #'ein:worksheet-copy-cell
    "nR" #'ein:notebook-rename-command
    "n S" #'ein:worksheet-toggle-slideshow-view
    "n C-s" #'ein:notebook-save-notebook-command
    "n C-l" #'ein:worksheet-clear-all-output
    "n C-v" #'ein:worksheet-collapse-output
    ;; worksheet-wise
    "w1" #'ein:notebook-worksheet-open-1th
    "w2" #'ein:notebook-worksheet-open-2th
    "w3" #'ein:notebook-worksheet-open-3th
    "w4" #'ein:notebook-worksheet-open-4th
    "w5" #'ein:notebook-worksheet-open-5th
    "w6" #'ein:notebook-worksheet-open-6th
    "w7" #'ein:notebook-worksheet-open-7th
    "w8" #'ein:notebook-worksheet-open-8th
    "w9" #'ein:notebook-worksheet-open-last
    "ws" #'ein:notebook-scratchsheet-open
    "w+" #'ein:notebook-worksheet-insert-next
    "w-" #'ein:notebook-worksheet-delete)
  ;; keybindings for ipython notebook traceback mode
  (spacemacs/set-leader-keys-for-major-mode 'ein:traceback-mode
    "RET" 'ein:tb-jump-to-source-at-point-command
    "n" 'ein:tb-next-item
    "p" 'ein:tb-prev-item
    "q" 'bury-buffer))

(defun yxl-datascience/setup-jupyter-hydra ()
  (defhydra yxl-datascience/jupyter-hydra (:color blue :hint nil
                                                  :pre (setq which-key-inhibit t)
                                                  :post (setq which-key-inhibit nil))
    ("q" nil)
    ("h" ein:notebook-worksheet-open-prev-or-last "Prev worksheet")
    ("j" ein:worksheet-goto-next-input :color red "Next cell")
    ("k" ein:worksheet-goto-prev-input :color red "Prev cell")
    ("l" ein:notebook-worksheet-open-next-or-first "Next worksheet")
    ("H" ein:notebook-worksheet-move-prev "Nav prev worksheet")
    ("J" ein:worksheet-move-cell-down :color red "Move cell down")
    ("K" ein:worksheet-move-cell-up :color red "Move cell up")
    ("L" ein:notebook-worksheet-move-next "Nav next worksheet")))
