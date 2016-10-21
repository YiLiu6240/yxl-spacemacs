(defun yxl-ess/lsos ()
  "invoke lsos function, this function needs to be in environment"
  (interactive)
  (ess-execute "lsos()"))

(defun yxl-ess/lsdf ()
  "invoke lsos function, this function needs to be in environment"
  (interactive)
  (ess-execute "lsdf()"))

(defun yxl-ess/ess-setup ()
  "wrap my settings in a function"
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

(defun yxl-ess/R-hook ()
  (ess-set-style 'RStudio 'quiet)
  ;; indent with 4 spaces
  (setq evil-shift-width 4)
  (setq ess-indent-offset 4)
  ;; no fancy comment
  (setq comment-add 0)
  ;; nested ifelse
  (setq ess-align-nested-calls '("ifelse"))
  (setq ess-indent-with-fancy-comments nil))

(defun yxl-ess/ess-setup-imenu ()
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

(defun yxl-ess/ess-repl-2cols ()
  (interactive)
  (delete-other-windows)

  ;; create two cols
  (split-window-right)

  ;; create an empty R mode buffer
  (split-window-below -15)
  (windmove-down)
  (let ((newbuf (generate-new-buffer-name "untitled")))
    (switch-to-buffer newbuf)
    (R-mode))
  (windmove-up)

  ;; create ess process and start a helper buffer
  (windmove-right)
  (split-window-below 20)
  (windmove-down)
  (call-interactively 'R)
  (split-window-below -20)
  (ess-help "ls")

  ;; restore focus to main window
  (windmove-left)
  (windmove-up))

(defun yxl-ess/ess-repl-3cols ()
  (interactive)
  (delete-other-windows)

  ;; create three cols
  (split-window-right)
  (split-window-right)

  ;; create an empty R mode buffer
  (split-window-below -20)
  (windmove-down)
  (let ((newbuf (generate-new-buffer-name "untitled")))
    (switch-to-buffer newbuf)
    (R-mode))
  (windmove-up)

  ;; create a middle pane
  (windmove-right)
  (split-window-below -20)

  ;; create ess process and start a helper buffer
  (windmove-right)
  (call-interactively 'R)
  (split-window-below -20)
  (ess-help "ls")

  ;; restore focus to main window
  (windmove-left)
  (windmove-left))
