(defun yxl-datascience/insert-pipe ()
  (interactive)
  (insert "%>%"))

(defun yxl-dash-search-docset-firefox ()
  (interactive)
  (let ((counsel-dash-browser-func #'browse-url-firefox))
    (yxl-dash-search-docset)))

(defun yxl-dash-search-docset-chromium ()
  (interactive)
  (let ((counsel-dash-browser-func #'browse-url-chromium))
    (yxl-dash-search-docset)))

(defun yxl-doc-portal-chromium ()
  (interactive)
  (let ((browse-url-browser-function #'browse-url-chromium))
    (yxl-doc-portal)))

(defun yxl-dash-search-docset-helm ()
  "Use helm to select docstring first, becasue ivy uses mini-buffer
and in some situtations it will cause problems."
  (interactive)
  (helm-autoresize-mode t)
  (let* ((helm-autoresize-max-height 80)
         (helm-dash-docsets-path yxl-datascience-docset-path)
         (helm-dash-common-docsets
          (list (helm :sources (helm-build-sync-source "docset"
                                 :candidates (helm-dash-installed-docsets))
                      :prompt "which docset to use: "
                      :buffer "*helm-dash-choose-docset"))))
    ;; Nevertheless helm is slow AF, so for the proper part, still use counsel
    (counsel-dash)))

(defun yxl-datascience/R-hook ())

(defun yxl-datascience/ess-hook ()
  (setq-local comment-add 0))

(defun yxl-datascience/ess-setup-imenu ()
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

(defun yxl-datascience/ess-setup-lintr ()
  (with-eval-after-load 'flycheck
    (setq flycheck-lintr-linters
          (concat "with_defaults(assignment_linter=NULL, "
                  "camel_case_linter=NULL, "
                  "commented_code_linter=NULL, "
                  "infix_spaces_linter=NULL)"))))

(defun yxl-datascience/ess-setup-help ()
  ;; TODO: replace this following upstream changes
  (defun ess-help-mode-hack ()
;;; Largely ripped from more-mode.el,
;;; originally by Wolfgang Rupprecht wolfgang@mgm.mit.edu
    "Mode for viewing ESS help files.
Use SPC and DEL to page back and forth through the file.
Use `n'  and `p' to move to next and previous section,
    `s' to jump to a particular section;   `s ?' for help.
Use `q' to return to your ESS session; `x' to kill this buffer first.
The usual commands for evaluating ESS source are available.
Other keybindings are as follows:
\\{ess-help-mode-map}

A Hack to allow spacemacs leader-keys by replacing run-hooks with run-mode-hooks.
TODO: replace this following upstream changes."
    (interactive)
    (setq major-mode 'ess-help-mode)
    (setq mode-name "ESS Help")
    (use-local-map ess-help-mode-map)

;;; Keep <tabs> out of the code.
    (make-local-variable 'indent-tabs-mode)
    (setq indent-tabs-mode nil)

    (if ess-mode-syntax-table ;;set in advance by ess-setq-local
        (set-syntax-table ess-mode-syntax-table))

    (require 'easymenu)
    (easy-menu-define ess-help-mode-menu-map ess-help-mode-map
      "Menu keymap for ess-help mode." ess-help-mode-menu)
    (easy-menu-add ess-help-mode-menu-map ess-help-mode-map)

    ;; Add the keys for navigating among sections; this is done
    ;; dynamically since different languages (e.g. S vs R) have different
    ;; section headings.

    (setq ess-help-sec-map (make-sparse-keymap))
    (setq-local show-trailing-whitespace nil)

    (dolist (pair ess-help-sec-keys-alist)
      (define-key ess-help-sec-map (char-to-string (car pair))
        'ess-skip-to-help-section))
    (define-key ess-help-sec-map "?" 'ess-describe-sec-map)
    (define-key ess-help-sec-map ">" 'end-of-buffer)
    (define-key ess-help-sec-map "<" 'beginning-of-buffer)
    (define-key ess-help-mode-map "s" ess-help-sec-map)

    (run-mode-hooks 'ess-help-mode-hook))
  (with-eval-after-load 'ess-help
    (advice-add 'ess-help-mode :override #'ess-help-mode-hack))
  (spacemacs/set-leader-keys-for-major-mode 'ess-help-mode
    "h" #'ess-help
    "b" #'ess-display-help-in-browser
    "p" #'ess-display-package-index
    "v" #'ess-display-vignettes
    "w" #'ess-help-web-search))

(defun yxl-datascience/ess-setup-rdired ()
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
    "x" #'ess-redired-expunge)
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

(defun yxl-datascience/jupyter-help ()
  "Normal invoke calls `ein:pytools-request-help'.
When invoking with a prefix arg, manually input func."
  (interactive)
  (if current-prefix-arg
      (progn
        (let ((func (read-string "Enter func: ")))
          (ein:pytools-request-help (ein:get-kernel-or-error) func)))
    (ein:pytools-request-help (ein:get-kernel-or-error) (ein:object-at-point-or-error))))

(defun spacemacs/ein:worksheet-merge-cell-next ()
  (interactive)
  (ein:worksheet-merge-cell
   (ein:worksheet--get-ws-or-error) (ein:worksheet-get-current-cell) t t))

(defun spacemacs//concat-leader (key)
  (if dotspacemacs-major-mode-leader-key
      (concat dotspacemacs-major-mode-leader-key key)
    (concat "," key)))

(defun ein:notebook-save-notebook-override (notebook
                                            retry
                                            &optional callback cbargs)
  (let ((content (ein:content-from-notebook notebook)))
    (ein:events-trigger (ein:$notebook-events notebook)
                        'notebook_saving.Notebook)
    (ein:content-save content
                      #'ein:notebook-save-notebook-success
                      (list notebook callback cbargs)
                      #'ein:notebook-save-notebook-error
                      (list notebook))))

(defun ein:edit-cell-exit-override ()
  "Close the EIN source edit buffer, saving contents back to the
original notebook cell, unless being called via
`ein:edit-cell-abort'."
  (interactive)
  (let ((edit-buffer (current-buffer))
        (ws ein:src--ws)
        (cell ein:src--cell))
    (ein:remove-overlay)
    (when ein:src--allow-write-back
      (ein:edit-cell-save))
    (kill-buffer-and-window)))

(defun ein:worksheet-insert-cell-below-and-edit ()
  (interactive)
  (call-interactively #'ein:worksheet-insert-cell-below)
  (call-interactively #'ein:edit-cell-contents))

(defun ein:edit-cell-save-and-execute-and-exit ()
  (interactive)
  (call-interactively #'ein:edit-cell-save-and-execute)
  (call-interactively #'ein:edit-cell-exit))

(defun ein:worksheet-collapse-output ()
  "By default will collapse output (reversing
`ein:worksheet-set-output-visibility-all'). When invoke with a
prefix, uncollapse them"
  (interactive)
  (let ((current-prefix-arg (not current-prefix-arg)))
    (call-interactively #'ein:worksheet-set-output-visibility-all)))
