(defun yxl-ess/setup ()
  (setq ess-history-file nil)
  ;; no spaces around argument assignment
  (setq ess-R-argument-suffix "=")
  (setq ess-eval-visibly 'nowait)
  (setq ess-execute-in-process-buffer t)
  (setq ess-ask-for-ess-directory nil)
  (setq ess-own-style-list '((ess-indent-offset . 4)
                             (ess-offset-arguments . open-delim)
                             (ess-offset-arguments-newline . prev-line)
                             (ess-offset-block . prev-line)
                             (ess-offset-continued . straight)
                             (ess-align-nested-calls . '("ifelse"))
                             (ess-align-arguments-in-calls "function[     ]*(")
                             (ess-align-continuations-in-calls)
                             (ess-align-blocks)
                             (ess-indent-from-lhs arguments)
                             (ess-indent-from-chain-start . t)
                             (ess-indent-with-fancy-comments)))
  (setq ess-default-style 'OWN))

(defun yxl-ess/R-hook ()
  (setq evil-shift-width 4)
  ;; no fancy comment
  (setq comment-add 0))

(defun yxl-ess/setup-imenu ()
  (setq ess-imenu-S-generic-expression
        '(("Functions" "^\\(.+\\)[ \t\n]*=[ \t\n]*function[ ]*" 1)
          ("Functions" "^\\(.+\\)[ \t\n]*<-[ \t\n]*function[ ]*" 1)
          ("Classes" "^.*setClass(\\(.*\\)," 1)
          ("Coercions" "^.*setAs(\\([^,]+,[^,]*\\)," 1) ; show from and to
          ("Generics" "^.*setGeneric(\\([^,]*\\)," 1)
          ("Methods" "^.*set\\(Group\\|Replace\\)?Method(\\([^,]+,[^,]*\\)" 2)
          ;;
          ("Package" "^.*\\(library\\|require\\)(\\(.*\\)" 2)
          ("Data" "^\\(.+\\)[ \t\n]-*=[ \t\n]*\\(read\\|.*data\.frame\\).*(" 1)
          ("Data" "^\\(.+\\)[ \t\n]-*<-[ \t\n]*\\(read\\|.*data\.frame\\).*(" 1)
          ("Outline" "^\\(## .+\\)$" 1)
          ("Outline" "^\\(### .+\\)$" 1)
          ("Outline" "^\\(#### .+\\)$" 1))))

(defun yxl-ess/setup-lintr ()
  (with-eval-after-load 'flycheck
    (setq flycheck-lintr-linters
          (concat "with_defaults(assignment_linter=NULL, "
                  "camel_case_linter=NULL, "
                  "commented_code_linter=NULL, "
                  "infix_spaces_linter=NULL)"))))

(defun yxl-ess/set-leader-keys (mode)
  (spacemacs/set-leader-keys-for-major-mode mode
    ";" #'ess-execute
    "of" #'yxl-ess-call-useful-funcs
    "op" #'yxl-ess-call-atpoint-useful-funcs
    "os" #'yxl-ess-call-atpoint-str
    "oS" #'yxl-ess-call-atpoint-generic
    "R" #'yxl-ess-open-rstudio
    ;; send
    "sa" #'ess-switch-process
    "st" nil
    "sT" nil
    "sf" #'ess-eval-function
    "sF" #'ess-eval-function-and-go
    "sp" #'ess-eval-paragraph
    "sP" #'ess-eval-pipe-through-line
    "sU" #'ess-install-library
    "su" #'ess-load-library
    "sw" #'ess-set-working-directory
    "s:" #'ess-execute
    ;; R helpers
    "hh" #'ess-display-help-on-object
    "hH" #'ess-describe-object-at-point
    "ha" #'ess-display-help-apropos
    "hp" #'ess-display-package-index
    "hv" #'ess-display-vignettes
    "hw" #'ess-help-web-search
    ;; Developer bindings
    "dT" 'ess-build-tags-for-directory
    "ds" 'ess-set-style
    "dg" 'ess-dump-object-into-edit-buffer
    ;; TODO only show these bindings if we're in R-mode.
    "dl" 'ess-r-devtools-load-package
    "dp" 'ess-r-devtools-set-pacakge
    "dt" 'ess-r-devtools-test-pacakge
    "dc" 'ess-r-devtools-check-pacakge
    "dr" 'ess-r-devtools-document-package
    "du" 'ess-r-devtools-unload-package
    "di" 'ess-r-devtools-install-package
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
    "vt" #'ess-R-dv-ctable
    "vd" #'ess-rdired))

(defun yxl-ess/declare-prefix (mode)
  (spacemacs/declare-prefix-for-mode mode "ms" "repl-interaction")
  (spacemacs/declare-prefix-for-mode mode "mh" "help")
  (spacemacs/declare-prefix-for-mode mode "md" "developer")
  (spacemacs/declare-prefix-for-mode mode "mb" "debugging")
  (spacemacs/declare-prefix-for-mode mode "mv" "views")
  (spacemacs/declare-prefix-for-mode mode "mr" "roxygen")
  (spacemacs/declare-prefix-for-mode mode "mo" "user-defined"))

(defun yxl-ess/setup-rdired ()
  (evilified-state-evilify ess-rdired-mode ess-rdired-mode-map
    "s" #'yxl-ess-rdired-str
    "S" #'ess-rdired-sort
    "v" #'ess-rdired-view
    "V" #'ess-rdired-View
    "g" #'revert-buffer))
