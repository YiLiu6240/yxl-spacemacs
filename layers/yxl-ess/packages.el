(setq yxl-ess-packages '(ess))

(defun yxl-ess/yxl-ess-settings ()
  "wrap my settings in a function"
  ;; disable "<-" assignment when pressing "_"
  (ess-toggle-underscore nil)
  ;; --------
  ;; no history file
  (setq ess-history-file nil)
  ;; no spaces around argument assignment
  (setq ess-R-argument-suffix "=")
  ;; speed ess eval efficiency
  (setq ess-eval-visibly 'nowait)
  ;; show results in buffer when calling ess-execute
  (setq ess-execute-in-process-buffer t)
  ;; Start R in the working directory by default
  (setq ess-ask-for-ess-directory nil))

(defun yxl-ess/yxl-R-hook ()
  (ess-set-style 'RStudio 'quiet)
  ;; indent with 4 spaces
  (setq evil-shift-width 4)
  (setq ess-indent-offset 4)
  ;; no fancy comment
  (setq comment-add 0)
  ;; nested ifelse
  (setq ess-align-nested-calls '("ifelse"))
  (setq ess-indent-with-fancy-comments nil))

(defun yxl-ess/post-init-ess ()
  ;; personal preferences
  (with-eval-after-load 'ess-site
    (add-hook 'R-mode-hook #'yxl-ess/yxl-R-hook)
    (add-hook 'ess-mode-hook #'yxl-ess/yxl-ess-hook)
    (add-hook 'ess-mode-hook 'smartparens-mode)
    (add-hook 'ess-mode-hook 'fci-mode)
    (add-hook 'ess-mode-hook 'hl-todo-mode)
    (add-hook 'ess-mode-hook 'which-function-mode)
    (yxl-ess/yxl-ess-settings))

  (with-eval-after-load 'flycheck
   (setq flycheck-lintr-linters
        (concat "with_defaults(assignment_linter=NULL, "
                "camel_case_linter=NULL, "
                "commented_code_linter=NULL, "
                "infix_spaces_linter=NULL)")))

  ;; ess self functions
  (defun yxl/ess-lsos ()
    "invoke lsos function, this function needs to be in environment"
    (interactive)
    (ess-execute "lsos()"))
  (defun yxl/ess-lsdf ()
    "invoke lsos function, this function needs to be in environment"
    (interactive)
    (ess-execute "lsdf()"))

  ;; overwrite: change process function
  (add-hook
   'ess-mode-hook
   (lambda ()
     (spacemacs/set-leader-keys-for-major-mode 'ess-mode
       "si" 'ess-switch-to-inferior-or-script-buffer)))

  ;; ess mode leader key bindings
  (spacemacs/set-leader-keys-for-major-mode 'ess-mode
    ",e" 'ess-execute
    ",d" 'ess-rdired
    ",fs" 'yxl/ess-lsos
    ",ff" 'yxl/ess-lsdf
    ",h" 'ess-help)

  ;; ess outline minor mode
  (with-eval-after-load 'ess-mode
    ;; ess definitions for symbols
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
            ("Outline" "^\\(#### .+\\)$" 1)))

    (add-hook 'ess-mode-hook 'outline-minor-mode)
    (setq outline-regexp
          "\\(#\\{2,3\\} \\)\\|\\([a-zA-Z0-9_\.]+ ?= ?function(.*$\\)")
    (defun outline-level ()
      (cond ((looking-at "### ") 1)
            ((looking-at "## ") 2)
            ((looking-at "[a-zA-Z0-9_\.]+ ?= ?function(.*$") 3)
            (t 1000))))

  ;; asb inspect object functions
  (with-eval-after-load 'ess-mode
    (require 'popup)

    (defun asb-read-into-string (buffer)
      (with-current-buffer buffer
        (buffer-string)))

    (defun asb-ess-R-object-popup (r-func)
      "R-FUNC: The R function to use on the object.
Run R-FUN for object at point, and display results in a popup."
      (let ((objname (current-word))
            (tmpbuf (get-buffer-create "**ess-R-object-popup**")))
        (if objname
            (progn
              (ess-command (concat "class(" objname ")\n") tmpbuf)
              (let ((bs (asb-read-into-string tmpbuf)))
                (if (not(string-match "\(object .* not found\)\|unexpected" bs))
                    (progn
                      (ess-command (concat r-func "(" objname ")\n") tmpbuf)
                      (let ((bs (asb-read-into-string tmpbuf)))
                        (popup-tip bs)))))))
        (kill-buffer tmpbuf)))

    (defun asb-ess-R-object-popup-str ()
      (interactive)
      (asb-ess-R-object-popup "str"))

    (defun asb-ess-R-object-popup-interactive (r-func)
      (interactive "sR function to execute: ")
      (asb-ess-R-object-popup r-func))

    (spacemacs/set-leader-keys-for-major-mode 'ess-mode
      ",i" 'asb-ess-R-object-popup-str
      ",I" 'asb-ess-R-object-popup-interactive))

  ;; ess-rdired hack
  (with-eval-after-load 'ess-mode
    (defun ess-rdired-str ()
      (interactive)
      (let ((objname (ess-rdired-object)))
        (ess-execute (concat "str(" objname ")\n"))))
    (add-hook
     'ess-rdired-mode-hook (lambda ()
                             (local-set-key "s" 'ess-rdired-str)
                             (local-set-key "S" 'ess-rdired-sort)))))
