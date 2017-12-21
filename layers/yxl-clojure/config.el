(spacemacs|define-jump-handlers clojure-mode)
(spacemacs|define-jump-handlers clojurec-mode)
(spacemacs|define-jump-handlers clojurescript-mode)
(spacemacs|define-jump-handlers clojurex-mode)
(spacemacs|define-jump-handlers cider-repl-mode)

;; TODO: Deprecate this after spacemacs major update
(spacemacs|defvar-company-backends cider-mode)
(spacemacs|defvar-company-backends cider-repl-mode)

(defvar clojure-enable-fancify-symbols nil
  "If non nil the `fancify-symbols' function is enabled.")
