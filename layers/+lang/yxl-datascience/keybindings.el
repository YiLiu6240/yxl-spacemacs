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
