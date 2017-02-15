(spacemacs/declare-prefix-for-mode 'ess-mode "mo" "user-defined")
(spacemacs/declare-prefix-for-mode 'inferior-ess-mode "ms" "repl-interaction")
(spacemacs/declare-prefix-for-mode 'inferior-ess-mode "mh" "help")
(spacemacs/declare-prefix-for-mode 'inferior-ess-mode "md" "developer")
(spacemacs/declare-prefix-for-mode 'inferior-ess-mode "mb" "debugging")
(spacemacs/declare-prefix-for-mode 'inferior-ess-mode "mv" "views")
(spacemacs/declare-prefix-for-mode 'inferior-ess-mode "mr" "roxygen")
(spacemacs/set-leader-keys-for-major-mode 'ess-mode
  "ofs" #'yxl-ess-exec-lsos
  "off" #'yxl-ess-exec-lsdf)
(spacemacs/set-leader-keys-for-major-mode 'ess-mode
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
  "bw" 'ess-watch)
(spacemacs/set-leader-keys-for-major-mode 'ess-mode
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
  "di" 'ess-r-devtools-install-package)
(spacemacs/set-leader-keys-for-major-mode 'ess-mode
  "hh" #'ess-help
  "hH" #'ess-describe-object-at-point
  "ha" #'ess-display-help-apropos
  "hp" #'ess-display-package-index
  "hv" #'ess-display-vignettes
  "hw" #'ess-help-web-search)
(spacemacs/set-leader-keys-for-major-mode 'ess-mode
  ;; roxygen
  "rh" 'ess-roxy-hide-all
  "rr" 'ess-roxy-update-entry
  "rn" 'ess-roxy-next-entry
  "rp" 'ess-roxy-previous-entry
  "rP" 'ess-roxy-preview-text
  "rt" 'ess-roxy-toggle-hiding)
(spacemacs/set-leader-keys-for-major-mode 'ess-mode
  ";" #'ess-execute
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
  "s:" #'ess-execute)
(spacemacs/set-leader-keys-for-major-mode 'ess-mode
  "vi" #'asb-ess-R-object-popup-str
  "vI" #'asb-ess-R-object-popup-interactive
  "vp" #'ess-R-dv-pprint
  "vt" #'ess-R-dv-ctable
  "vd" #'ess-rdired
  "vs" #'yxl-ess-at-point-str
  "vS" #'yxl-ess-at-point-generic)

(spacemacs/declare-prefix-for-mode 'inferior-ess-mode "ms" "repl-interaction")
(spacemacs/declare-prefix-for-mode 'inferior-ess-mode "mh" "help")
(spacemacs/declare-prefix-for-mode 'inferior-ess-mode "md" "developer")
(spacemacs/declare-prefix-for-mode 'inferior-ess-mode "mb" "debugging")
(spacemacs/declare-prefix-for-mode 'inferior-ess-mode "mv" "views")
(spacemacs/declare-prefix-for-mode 'inferior-ess-mode "mr" "roxygen")

(spacemacs/set-leader-keys-for-major-mode 'inferior-ess-mode
  ";" #'ess-execute

  "sU" #'ess-install-library
  "su" #'ess-load-library
  "s:" #'ess-execute
  "sw" #'ess-set-working-directory
  ;; R helpers
  "hh" #'ess-display-help-on-object
  "hH" #'ess-describe-object-at-point
  "ha" #'ess-display-help-apropos
  "hp" #'ess-display-package-index
  "hv" #'ess-display-vignettes
  "hw" #'ess-help-web-search

  ;; Developer bindings
  "dg" #'ess-dump-object-into-edit-buffer

  ;; TODO only show these bindings if we're in R-mode.
  "dl" #'ess-r-devtools-load-package
  "dp" #'ess-r-devtools-set-pacakge
  "dt" #'ess-r-devtools-test-pacakge
  "dc" #'ess-r-devtools-check-pacakge
  "dr" #'ess-r-devtools-document-package
  "du" #'ess-r-devtools-unload-package
  "di" #'ess-r-devtools-install-package

  ;; debug bindings
  "bT" #'ess-show-traceback
  "be" #'ess-debug-toggle-error-action
  "bw" #'ess-watch
  ;; other views
  "vi" #'asb-ess-R-object-popup-str
  "vI" #'asb-ess-R-object-popup-interactive
  "vp" #'ess-R-dv-pprint
  "vt" #'ess-R-dv-ctable
  "vd" #'ess-rdired)

(with-eval-after-load 'ess-site
  (evilified-state-evilify ess-rdired-mode ess-rdired-mode-map
    "s" #'yxl-ess-rdired-str
    "S" #'ess-rdired-sort
    "v" #'ess-rdired-view
    "V" #'ess-rdired-View
    "g" #'revert-buffer))
