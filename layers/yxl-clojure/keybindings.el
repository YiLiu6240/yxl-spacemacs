(defun yxl-clojure/setup-clojure-keybindings ()
  (define-key clojure-mode-map
    (kbd "C-c f") #'yxl-prog/evil-wrap-line-f-lisp-print)
  (define-key clojure-mode-map
    (kbd "C-c F") #'yxl-prog/evil-wrap-line-f-lisp))

(defun yxl-clojure/setup-cider-leader-keys ()
  ;; cider-repl-mode only
  (spacemacs/set-leader-keys-for-major-mode 'cider-repl-mode
    "," 'cider-repl-handle-shortcut)
  (spacemacs/set-leader-keys-for-major-mode 'cider-clojure-interaction-mode
    "ep" 'cider-eval-print-last-sexp)
  ;; TODO: having this work for cider-macroexpansion-mode would be nice,
  ;;       but the problem is that it uses clojure-mode as its major-mode
  (let ((cider--key-binding-prefixes
         '(("md" . "debug")
           ("me" . "evaluation")
           ("mg" . "goto")
           ("mh" . "documentation")
           ("ms" . "repl")
           ("mt" . "test")
           ("mT" . "toggle")
           ("mf" . "format"))))
    (dolist (m '(clojure-mode
                 clojurec-mode
                 clojurescript-mode
                 clojurex-mode
                 cider-repl-mode
                 cider-clojure-interaction-mode))
      (mapc (lambda (x) (spacemacs/declare-prefix-for-mode
                          m (car x) (cdr x)))
            cider--key-binding-prefixes)

      (spacemacs/set-leader-keys-for-major-mode m
        "ha" 'cider-apropos
        "hh" 'cider-doc
        "hg" 'cider-grimoire
        "hj" 'cider-javadoc
        "hn" 'cider-browse-ns
        "hc" 'clojure-cheatsheet

        "e;" 'cider-eval-defun-to-comment
        "eb" 'cider-eval-buffer
        "ec" #'clojure/cider-eval-current-form-sp
        "ee" 'cider-eval-last-sexp
        "ef" 'cider-eval-defun-at-point
        "em" 'cider-macroexpand-1
        "eM" 'cider-macroexpand-all
        "er" 'cider-eval-region
        "eP" 'cider-pprint-eval-last-sexp
        "ew" 'cider-eval-last-sexp-and-replace

        "="  'cider-format-buffer
        "fb" 'cider-format-buffer

        "gb" 'cider-pop-back
        "gc" 'cider-classpath
        "ge" 'cider-jump-to-compilation-error
        "gr" 'cider-jump-to-resource
        "gn" 'cider-browse-ns
        "gN" 'cider-browse-ns-all

        "'"  'cider-jack-in
        "\"" 'cider-jack-in-clojurescript
        "sb" 'cider-load-buffer
        "sB" 'spacemacs/cider-send-buffer-in-repl-and-focus
        "sc" #'clojure/cider-send-current-form-sp-to-repl
        "sC-c" (if (eq m 'cider-repl-mode)
                   'cider-repl-clear-buffer
                 'cider-connect)
        "sC-C" 'cider-find-and-clear-repl-output
        "se" 'spacemacs/cider-send-last-sexp-to-repl
        "sE" 'spacemacs/cider-send-last-sexp-to-repl-focus
        "sf" 'spacemacs/cider-send-function-to-repl
        "sF" 'spacemacs/cider-send-function-to-repl-focus
        "si" 'cider-jack-in
        "sI" 'cider-jack-in-clojurescript
        "sn" 'spacemacs/cider-send-ns-form-to-repl
        "sN" 'spacemacs/cider-send-ns-form-to-repl-focus
        "so" 'cider-repl-switch-to-other
        "sq" 'cider-quit
        "sr" 'spacemacs/cider-send-region-to-repl
        "sR" 'spacemacs/cider-send-region-to-repl-focus
        "ss" (if (eq m 'cider-repl-mode)
                 'cider-switch-to-last-clojure-buffer
               'cider-switch-to-repl-buffer)
        "sx" 'cider-refresh
        "sX" 'cider-restart

        "Te" 'cider-enlighten-mode
        "Tf" 'spacemacs/cider-toggle-repl-font-locking
        "Tp" 'spacemacs/cider-toggle-repl-pretty-printing
        "Tt" 'cider-auto-test-mode

        "ta" 'spacemacs/cider-test-run-all-tests
        "tb" 'cider-test-show-report
        "tl" 'spacemacs/cider-test-run-loaded-tests
        "tp" 'spacemacs/cider-test-run-project-tests
        "tn" 'spacemacs/cider-test-run-ns-tests
        "tr" 'spacemacs/cider-test-rerun-failed-tests
        "tt" 'spacemacs/cider-test-run-focused-test

        "db" 'cider-debug-defun-at-point
        "de" 'spacemacs/cider-display-error-buffer
        "dv" 'cider-inspect
        ;; refactorings from clojure-mode
        "rc{" 'clojure-convert-collection-to-map
        "rc(" 'clojure-convert-collection-to-list
        "rc'" 'clojure-convert-collection-to-quoted-list
        "rc#" 'clojure-convert-collection-to-set
        "rc[" 'clojure-convert-collection-to-vector
        "rc:" 'clojure-toggle-keyword-string))))

(defun yxl-clojure/setup-cider-evilified-keybindings ()
  (evilified-state-evilify cider-stacktrace-mode cider-stacktrace-mode-map
    (kbd "C-n") 'cider-stacktrace-next-cause
    (kbd "C-p") 'cider-stacktrace-previous-cause
    (kbd "TAB") 'cider-stacktrace-cycle-current-cause
    (kbd "0")   'cider-stacktrace-cycle-all-causes
    (kbd "1")   'cider-stacktrace-cycle-cause-1
    (kbd "2")   'cider-stacktrace-cycle-cause-2
    (kbd "3")   'cider-stacktrace-cycle-cause-3
    (kbd "4")   'cider-stacktrace-cycle-cause-4
    (kbd "5")   'cider-stacktrace-cycle-cause-5
    (kbd "a")   'cider-stacktrace-toggle-all
    (kbd "c")   'cider-stacktrace-toggle-clj
    (kbd "d")   'cider-stacktrace-toggle-duplicates
    (kbd "J")   'cider-stacktrace-toggle-java
    (kbd "r")   'cider-stacktrace-toggle-repl
    (kbd "T")   'cider-stacktrace-toggle-tooling)
  (evilified-state-evilify cider-docview-mode cider-docview-mode-map
    (kbd "q") 'cider-popup-buffer-quit)
  (evilified-state-evilify cider-inspector-mode cider-inspector-mode-map
    (kbd "L") 'cider-inspector-pop
    (kbd "n") 'cider-inspector-next-page
    (kbd "N") 'cider-inspector-prev-page
    (kbd "p") 'cider-inspector-prev-page
    (kbd "r") 'cider-inspector-refresh)
  (evilified-state-evilify cider-test-report-mode cider-test-report-mode-map
    (kbd "C-n") 'cider-test-next-result
    (kbd "C-p") 'cider-test-previous-result
    (kbd "RET") 'cider-test-jump
    (kbd "d")   'cider-test-ediff
    (kbd "e")   'cider-test-stacktrace
    (kbd "q")   'cider-popup-buffer-quit
    (kbd "r")   'cider-test-rerun-tests
    (kbd "t")   'cider-test-run-test
    (kbd "T")   'cider-test-run-ns-tests)
  (evil-define-key 'normal cider-repl-mode-map
    (kbd "C-n") 'cider-repl-next-input
    (kbd "C-p") 'cider-repl-previous-input)
  (evil-define-key 'insert cider-repl-mode-map
    (kbd "C-n") 'cider-repl-next-input
    (kbd "C-p") 'cider-repl-previous-input))

(defun yxl-clojure/setup-sayid-leader-keys ()
  (setq sayid--key-binding-prefixes
        '(("mdt" . "trace")))
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode
               cider-repl-mode
               cider-clojure-interaction-mode))
    (mapc (lambda (x) (spacemacs/declare-prefix-for-mode
                        m (car x) (cdr x)))
          sayid--key-binding-prefixes)
    (spacemacs/set-leader-keys-for-major-mode m
      ;;These keybindings mostly preserved from the default sayid bindings
      "df" 'sayid-query-form-at-point
      "dw" 'sayid-get-workspace
      "dE" 'sayid-eval-last-sexp ;in default sayid bindings this is lowercase e, but that was already used in clojure mode
      "d!" 'sayid-load-enable-clear
      "dc" 'sayid-clear-log
      "dx" 'sayid-reset-workspace
      "ds" 'sayid-show-traced
      "dS" 'sayid-show-traced-ns
      "dV" 'sayid-set-view
      "dh" 'sayid-show-help
      "dty" 'sayid-trace-all-ns-in-dir
      "dtp" 'sayid-trace-ns-by-pattern
      "dtb" 'sayid-trace-ns-in-file
      "dte" 'sayid-trace-fn-enable
      "dtE" 'sayid-trace-enable-all
      "dtd" 'sayid-trace-fn-disable
      "dtD" 'sayid-trace-disable-all
      "dtn" 'sayid-inner-trace-fn
      "dto" 'sayid-outer-trace-fn
      "dtr" 'sayid-remove-trace-fn
      "dtK" 'sayid-kill-all-traces)))

(defun yxl-clojure/setup-sayid-evilified-keybindings ()
  (evilified-state-evilify sayid-mode sayid-mode-map
    (kbd "H") 'sayid-buf-show-help
    (kbd "n") 'sayid-buffer-nav-to-next
    (kbd "N") 'sayid-buffer-nav-to-prev
    (kbd "C-s v") 'sayid-toggle-view
    (kbd "C-s V") 'sayid-set-view
    (kbd "L") 'sayid-buf-back
    (kbd "e") 'sayid-gen-instance-expr) ;Originally this was bound to 'g', but I feel this is still mnemonic and doesn't overlap with evil
  (evilified-state-evilify sayid-pprint-mode sayid-pprint-mode-map
    (kbd "h") 'sayid-pprint-buf-show-help
    (kbd "n") 'sayid-pprint-buf-next
    (kbd "N") 'sayid-pprint-buf-prev
    (kbd "l") 'sayid-pprint-buf-exit)
  (evilified-state-evilify sayid-traced-mode sayid-traced-mode-map
    (kbd "l") 'sayid-show-traced
    (kbd "h") 'sayid-traced-buf-show-help))
