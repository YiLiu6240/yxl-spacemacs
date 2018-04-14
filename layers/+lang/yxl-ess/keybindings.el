(defun yxl-ess/setup-general-keybindings ()
  ;; move "<-" key to "C-c ="
  (setq ess-S-assign "<-")
  (setq ess-smart-S-assign-key (kbd "C-c ="))
  (ess-toggle-S-assign nil)
  (ess-toggle-S-assign nil)

  (define-key ess-mode-map (kbd "C-c C-.") #'yxl-ess/insert-pipe)
  (define-key ess-mode-map (kbd "C-<tab>") #'sp-indent-adjust-sexp)
  (define-key ess-mode-map (kbd "C-S-<tab>") #'sp-dedent-adjust-sexp)
  (define-key ess-mode-map (kbd "<s-return>") 'ess-eval-line)
  (define-key ess-doc-map "h" 'ess-display-help-on-object)
  (define-key ess-doc-map "p" 'ess-R-dv-pprint)
  (define-key ess-doc-map "t" 'ess-R-dv-ctable)
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

(defun yxl-ess/setup-leader-keys ()
  (dolist (mode '(ess-mode ess-julia-mode inferior-ess-mode))
    (spacemacs/declare-prefix-for-mode mode "ma" "atpoint")
    (spacemacs/declare-prefix-for-mode mode "mb" "debugging")
    (spacemacs/declare-prefix-for-mode mode "mc" "noweb")
    (spacemacs/declare-prefix-for-mode mode "md" "dev")
    (spacemacs/declare-prefix-for-mode mode "mD" "developer")
    (spacemacs/declare-prefix-for-mode mode "mh" "help")
    (spacemacs/declare-prefix-for-mode mode "mr" "extra")
    (spacemacs/declare-prefix-for-mode mode "ms" "repl")
    (spacemacs/declare-prefix-for-mode mode "mv" "views")
    (spacemacs/declare-prefix-for-mode mode "mw" "pkg")
    (spacemacs/declare-prefix-for-mode mode "mo" "user-defined")
    (spacemacs/set-leader-keys-for-major-mode mode
      ","  'ess-eval-region-or-function-or-paragraph-and-step
      ";" #'ess-execute
      "'"  'spacemacs/ess-start-repl
      "a" #'yxl-ess-atpoint
      "A" #'yxl-ess-atpoint-pop
      ;; noweb
      "cC" 'ess-eval-chunk-and-go
      "cc" 'ess-eval-chunk
      "cd" 'ess-eval-chunk-and-step
      "cm" 'ess-noweb-mark-chunk
      "cN" 'ess-noweb-previous-chunk
      "cn" 'ess-noweb-next-chunk
      "d" #'ess-rdired
      "D" 'ess-dev-map
      "i" #'yxl-ess-repl-popup
      ;; R helpers
      "hh" #'ess-help
      "hH" #'ess-display-help-on-object
      "ha" #'ess-display-help-apropos
      "hb" #'ess-display-help-in-browser
      "hi" #'ess-R-object-popup
      "hp" #'ess-display-package-index
      "hv" #'ess-display-vignettes
      "hw" #'ess-help-web-search
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
      ;; send
      "sa" #'ess-switch-process
      "sb" 'ess-eval-buffer
      "sB" 'ess-eval-buffer-and-go
      "sd" 'ess-eval-function-or-paragraph-and-step
      "sf" #'ess-eval-function
      "sF" #'ess-eval-function-and-go
      "si" 'spacemacs/ess-start-repl
      "sL" 'ess-eval-line-and-go
      "sl" 'ess-eval-line
      "sp" #'ess-eval-paragraph-and-step
      "sP" #'ess-eval-pipe-through-line
      "sr" 'ess-eval-region
      "sR" 'ess-eval-region-and-go
      "ss" 'ess-eval-region-or-line-and-step
      "s C-s" 'ess-switch-to-inferior-or-script-buffer
      "sU" #'ess-install-library
      "su" #'ess-load-library
      "sw" #'ess-set-working-directory
      "s:" #'ess-execute
      ;; others
      "of" #'yxl-ess-call-useful-funcs
      "os" #'yxl-ess-call-atpoint-str
      "oS" #'yxl-ess-call-atpoint-generic
      "r" 'ess-extra-map
      "R" #'yxl-ess-open-rstudio
      ;; other views
      "vi" #'asb-ess-R-object-popup-str
      "vI" #'asb-ess-R-object-popup-interactive
      "vp" #'ess-R-dv-pprint
      "vt" #'ess-R-dv-ctable
      ;; w
      "w" 'ess-r-package-dev-map)))

(defun yxl-ess/setup-julia-keybindings ()
  (spacemacs/set-leader-keys-for-major-mode 'ess-julia-mode
    "<tab>" #'julia-latexsub-or-indent))

(defun yxl-ess/setup-ess-rdired-leader-keys ()
  (spacemacs/set-leader-keys-for-major-mode 'ess-rdired-mode
    "a" #'yxl-ess-rdired-atpoint
    "s" #'yxl-ess-rdired-str
    "S" #'ess-rdired-sort
    "vv" #'ess-redired-view
    "vp" #'ess-R-dv-pprint
    "vd" #'ess-view-inspect-df
    "vt" #'ess-R-dv-ctable
    "g" #'revert-buffer
    "p" #'ess-rdired-plot
    "y" #'ess-rdired-type
    "d" #'ess-rdired-delete
    "u" #'ess-rdired-undelete
    "x" #'ess-redired-expunge))

(defun yxl-ess/setup-ess-help-leader-keys ()
  (spacemacs/set-leader-keys-for-major-mode 'ess-help-mode
    "h" #'ess-help
    "b" #'ess-display-help-in-browser
    "p" #'ess-display-package-index
    "v" #'ess-display-vignettes
    "w" #'ess-help-web-search))
