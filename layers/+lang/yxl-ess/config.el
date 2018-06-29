(defvar yxl-ess-enable-lsp nil
  "Enable language server protocol for ess.")

(defun yxl-ess//setup-general-configs ()
  ;; Follow Hadley Wickham's R style guide
  (setq ess-default-style 'RStudio)
  ;; (setq ess-nuke-trailing-whitespace-p t)
  (setq ess-history-file nil)
  (setq ess-R-argument-suffix " = ")
  (setq ess-eval-visibly 'nowait)
  (setq ess-execute-in-process-buffer t)
  (setq ess-ask-for-ess-directory nil)
  (setq ess-use-flymake nil)
  (setq ess-use-auto-complete nil)
  (setq ess-fl-keyword:operators (cons "[-=+></%$!(::)]+"
                                       'font-lock-constant-face))
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
          (ess-R-fl-keyword:F&T . t))))

(defun yxl-ess//setup-imenu ()
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
          ("Knitr-Outline" "^#' \\(#+ .+\\)$" 1)
          ("Knitr-chunk" "^#\\+ \\(.+\\)$" 1)
          ("FALSE block" "^\\(if (FALSE) {.*\\)$" 1))))

(defun yxl-ess//setup-lintr ()
  (with-eval-after-load 'flycheck
    (setq flycheck-lintr-linters
          (concat "with_defaults(assignment_linter=NULL, "
                  "object_name_linter=NULL, "
                  "object_usage_linter=NULL, "
                  "camel_case_linter=NULL, "
                  "commented_code_linter=NULL, "
                  "infix_spaces_linter=NULL)"))))

(defun yxl-ess//setup-rdired ()
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
