(defun yxl-ess/ess-setup ()
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
