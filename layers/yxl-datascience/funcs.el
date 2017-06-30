(defun yxl-datascience/setup-generic ()
  (setq ess-history-file nil)
  ;; no spaces around argument assignment
  (setq ess-R-argument-suffix " = ")
  (setq ess-eval-visibly 'nowait)
  (setq ess-execute-in-process-buffer t)
  (setq ess-ask-for-ess-directory nil)
  (setq yxl-ess-style '((ess-indent-offset . 2)
                        (ess-offset-arguments . open-delim)
                        (ess-offset-arguments-newline . prev-call)
                        (ess-offset-block . prev-line)
                        (ess-offset-continued . straight)
                        (ess-align-nested-calls "ifelse")
                        (ess-align-arguments-in-calls "function[    ]*(")
                        (ess-align-continuations-in-calls . t)
                        (ess-align-blocks control-flow)
                        (ess-indent-from-lhs arguments fun-decl-opening)
                        (ess-indent-from-chain-start . t)
                        (ess-indent-with-fancy-comments)))
  (ess-add-style 'yxl-ess-style yxl-ess-style)
  (setq ess-default-style 'yxl-ess-style)
  (setq ess-fl-keyword:operators (cons "[-=+></%$!(::)]+" 'font-lock-constant-face))
  (setq ess-R-font-lock-keywords
        '((ess-R-fl-keyword:modifiers  . t)
          (ess-R-fl-keyword:fun-defs   . t)
          (ess-R-fl-keyword:keywords   . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:constants  . t)
          (ess-fl-keyword:fun-calls . t)
          (ess-fl-keyword:numbers . t)
          (ess-fl-keyword:operators . t)
          (ess-fl-keyword:delimiters . t)
          (ess-fl-keyword:= . t)
          (ess-R-fl-keyword:F&T . t)
          (ess-R-fl-keyword:%op% . t))))

(defun yxl-datascience/insert-pipe ()
  (interactive)
  (insert "%>%"))

(defun yxl-datascience/setup-bindings ()
  ;; move "<-" key to "C-c ="
  (setq ess-S-assign "<-")
  (setq ess-smart-S-assign-key (kbd "C-c ="))
  (ess-toggle-S-assign nil)
  (ess-toggle-S-assign nil)

  (define-key ess-mode-map (kbd "C-c C-.") #'yxl-datascience/insert-pipe)
  (define-key ess-mode-map (kbd "C-<tab>") #'sp-indent-adjust-sexp)
  (define-key ess-mode-map (kbd "C-S-<tab>") #'sp-dedent-adjust-sexp)
  (evil-define-key 'normal inferior-ess-mode-map
    (kbd "C-d") #'evil-scroll-down)
  (define-key ess-mode-map (kbd "C-,") #'ess-eval-region-or-line-and-step))

(defun yxl-datascience/R-hook ())

(defun yxl-datascience/ess-hook ()
  (setq-local comment-add 0))

(defun yxl-datascience/ess-set-style-advice (&optional style quiet)
  "Set `evil-shift-width' based on `ess-indent-offset'.")
;; (setq-local evil-shift-width ess-indent-offset)

(defun yxl-datascience/setup-imenu ()
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
          ("Outline" "^[ \t]*\\(# .+\\)----.*$" 1)
          ("Outline" "^[ \t]*\\(# .+\\)====.*$" 1)
          ("FALSE block" "^\\(if (FALSE) {.*\\)$" 1))))

(defun yxl-datascience/setup-lintr ()
  (with-eval-after-load 'flycheck
    (setq flycheck-lintr-linters
          (concat "with_defaults(assignment_linter=NULL, "
                  "camel_case_linter=NULL, "
                  "commented_code_linter=NULL, "
                  "infix_spaces_linter=NULL)"))))

(defun yxl-datascience/set-leader-keys (mode)
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
    "sp" #'ess-eval-paragraph
    "sP" #'ess-eval-pipe-through-line
    "sU" #'ess-install-library
    "su" #'ess-load-library
    "sw" #'ess-set-working-directory
    "s:" #'ess-execute
    ;; R helpers
    "hh" #'ess-display-help-on-object
    "hb" #'ess-display-help-in-browser
    "hH" #'ess-describe-object-at-point
    "ha" #'ess-display-help-apropos
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

(defun yxl-datascience/declare-prefix (mode)
  (spacemacs/declare-prefix-for-mode mode "ma" "atpoint")
  (spacemacs/declare-prefix-for-mode mode "ms" "repl-interaction")
  (spacemacs/declare-prefix-for-mode mode "mh" "help")
  (spacemacs/declare-prefix-for-mode mode "mD" "developer")
  (spacemacs/declare-prefix-for-mode mode "mb" "debugging")
  (spacemacs/declare-prefix-for-mode mode "mv" "views")
  (spacemacs/declare-prefix-for-mode mode "mr" "roxygen")
  (spacemacs/declare-prefix-for-mode mode "mo" "user-defined"))

(defun yxl-datascience/setup-ess-help ()
  (defun yxl-datascience/ess-help-config ()
    (define-key ess-help-mode-map "." #'yxl-ess-help-hydra/body))
  (add-hook 'ess-help-mode-hook #'yxl-datascience/ess-help-config)
  (defhydra yxl-ess-help-hydra (:color blue :hint nil :columns 4
                                       :pre (setq which-key-inhibit t)
                                       :post (setq which-key-inhibit nil))
    ("h" ess-display-help-on-object "help")
    ("b" ess-display-help-in-browser "help-in-browser")
    ("p" ess-display-package-index "package index")
    ("v" ess-display-vignettes "vignettes")
    ("w" ess-help-web-search "web-search")))

(defun yxl-datascience/setup-rdired ()
  (defun yxl-datascience/rdired-config ()
    (define-key ess-rdired-mode-map "." #'yxl-ess-rdired-hydra/body)
    (define-key ess-rdired-mode-map "a" #'yxl-ess-rdired-atpoint)
    (define-key ess-rdired-mode-map "A" #'yxl-ess-rdired-atpoint-pop))
  (add-hook 'ess-rdired-mode-hook #'yxl-datascience/rdired-config)
  (defhydra yxl-ess-rdired-hydra (:color blue :hint nil :columns 4
                                         :pre (setq which-key-inhibit t)
                                         :post (setq which-key-inhibit nil))
    ("s" yxl-ess-rdired-str "str")
    ("S" ess-rdired-sort "sort")
    ;; view in REPL
    ("vv" ess-rdired-view "view")
    ;; view in its own buffer
    ("vp" ess-R-dv-pprint "dv:pprint")
    ("vd" ess-view-inspect-df "ess-view:inspect-df")
    ("vt" ess-R-dv-ctable "dv:ctable")
    ("g" revert-buffer "revert")
    ("a" yxl-ess-rdired-atpoint "useful-funcs")
    ("A" yxl-ess-rdired-atpoint-pop "useful-funcs:pop")
    ("p" ess-rdired-plot "plot")
    ("y" ess-rdired-type "mode(.)")
    ("d" ess-rdired-delete "delete")
    ("u" ess-rdired-undelete "undelete")
    ("x" ess-rdired-expunge "expunge"))
  (setq ess-rdired-objects "{.rdired.objects <- function(objs) {
  if (length(objs)==0) {
    \"No objects to view!\"
  } else {
  mode <- sapply(objs, function(my.x) {
    eval( parse( text=sprintf('data.class(get(\"%s\"))', my.x))) })
  length <- sapply(objs, function(my.x) {
    eval( parse( text=sprintf('length(get(\"%s\"))', my.x))) })
  nrow <- sapply(objs, function(my.x) {
    eval( parse( text=
      sprintf('if (is.null(nrow(get(\"%s\")))) {
                 \".\"
               } else {
                 nrow(get(\"%s\"))
               }', my.x, my.x))) })
  ncol <- sapply(objs, function(my.x) {
    eval( parse( text=
      sprintf('if (is.null(ncol(get(\"%s\")))) {
                 \".\"
               } else {
                 ncol(get(\"%s\"))
               }', my.x, my.x))) })
  size <- sapply(objs, function(my.x) {
    eval( parse( text=sprintf('format(object.size(get(\"%s\")),
                                      units=\"auto\")',
                               my.x))) })
  d <- data.frame(mode, length, nrow, ncol, size)
  var.names <- row.names(d)
  ## If any names contain spaces, we need to quote around them.
  quotes = rep('', length(var.names))
  spaces = grep(' ', var.names)
  if (any(spaces))
    quotes[spaces] <- '\"'
  var.names = paste(quotes, var.names, quotes, sep='')
  row.names(d) <- paste('  ', var.names, sep='')
  d[order(mode), ]
  }
}; cat('\n'); print(.rdired.objects(ls()))}\n"))

(defun yxl-datascience/setup-julia-bindings ()
  (spacemacs/set-leader-keys-for-major-mode 'ess-julia-mode
    "<tab>" #'julia-latexsub-or-indent))
