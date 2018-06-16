(defun evil-surround-str-block ()
  "Read a string block, and surround the selection with this block."
  (let ((str-block (read-from-minibuffer "" "")))
    (cons (format "%s" (or str-block ""))
          (format "%s" (or str-block "")))))

(defun evil-surround-str-block-space ()
  "Read a string block, and surround the selection with this block, padded
with spaces."
  (let ((str-block (read-from-minibuffer "" "")))
    (cons (format "%s " (or str-block ""))
          (format " %s" (or str-block "")))))

(defun evil-surround-str-block-pair ()
  "Read a string block, and surround the selection with this block, padded
with spaces."
  (let ((str-block-left (read-from-minibuffer "left-pair: " ""))
        (str-block-right (read-from-minibuffer "right-pair: " "")))
    (cons (format "%s" (or str-block-left ""))
          (format "%s" (or str-block-right "")))))

(defun yxl-evil/surround-function-print ()
  "Read a functionname from the minibuffer and wrap selection in function call"
  (let ((fname "print"))
    (cons (format "%s(" (or fname ""))
          ")")))

(defun evil-Evimrc ()
  "edit my vimrc."
  (interactive)
  (find-file "~/.vimrc"))

(defun yxl-evil//evil-surround-pairs ()
  "press viw then press the trigger key"
  (dolist (key '((?g . ("{" . "}"))
                 (?h . ("[" . "]"))
                 (?y . ("\"" . "\""))
                 (?u . ("'" . "'"))
                 (?m . ("\\\(" . "\\\)"))
                 (?M . ("\\\( " . " \\\)"))
                 (?n . ("\\[" . "\\]"))
                 (?N . ("\\[ " . " \\]"))
                 (?k . ("$ " . " $"))
                 (?K . ("$" . "$"))
                 (?F . yxl-evil/surround-function-print)
                 (?r . evil-surround-str-block)
                 (?R . evil-surround-str-block-space)
                 (?e . evil-surround-str-block-pair)))
    (add-to-list 'evil-surround-pairs-alist key)))

(defun yxl-evil//setup-evil-general ()
  (progn
    ;; vim-surround, use "S"
    ;; (define-key 'visual evil-surround-mode-map "s" #'evil-substitute)
    ;; (define-key 'visual evil-surround-mode-map "S" #'evil-surround-region)

    ;; normal and motion state:
    ;; "g" related commands
    ;; mark: repalce with evil-middle-of-visual-line
    (define-key evil-motion-state-map "gm" #'evil-goto-mark)

    ;; navigation
    (define-key evil-motion-state-map "{" 'yxl-evil-backward-paragraph)
    (define-key evil-motion-state-map "}" 'yxl-evil-forward-paragraph)
    (define-key evil-motion-state-map "j" 'evil-next-visual-line)
    (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
    (define-key evil-motion-state-map "gh" #'evil-first-non-blank)
    (define-key evil-motion-state-map "gl" #'evil-last-non-blank)

    ;; ex:
    ;; ":" and ";"
    (define-key evil-motion-state-map (kbd dotspacemacs-ex-command-key) 'evil-ex)
    ;; only bind ";" as ":" under normal mode
    (define-key evil-normal-state-map (kbd ";") 'evil-ex)

    ;; misc:
    ;; (define-key evil-motion-state-map (kbd "C-S-p") #'helm-M-x)
    (define-key evil-motion-state-map (kbd "C-S-p") #'counsel-M-x)
    (define-key evil-normal-state-map (kbd "_") #'projectile-dired)

    ;; from evil-goodies
    (define-key evil-motion-state-map "g<" #'yxl-evil-go-up-indent)))

(defun yxl-evil//setup-evil-insert-state ()
  ;; insert state:
  (define-key evil-insert-state-map
    (kbd "C-h") #'backward-delete-char-untabify)
  (define-key evil-insert-state-map (kbd "C-d") #'delete-forward-char)
  (define-key evil-insert-state-map (kbd "C-a") #'beginning-of-line-text)
  (define-key evil-insert-state-map (kbd "C-e") #'end-of-line)
  (define-key evil-insert-state-map (kbd "C-p") #'previous-line)
  (define-key evil-insert-state-map (kbd "C-n") #'next-line))

(defun yxl-evil//setup-evil-c-hjkl ()
  (define-key evil-motion-state-map (kbd "C-h") #'evil-window-left)
  (define-key evil-motion-state-map (kbd "C-j") #'evil-window-down)
  (define-key evil-motion-state-map (kbd "C-k") #'evil-window-up)
  (define-key evil-motion-state-map (kbd "C-l") #'evil-window-right)
  (with-eval-after-load 'comint
    (evil-define-key 'insert comint-mode-map
      (kbd "C-j") #'windmove-down
      (kbd "C-k") #'windmove-up
      (kbd "C-p") #'comint-previous-input
      (kbd "C-n") #'comint-next-input)
    (evil-define-key 'normal comint-mode-map
      (kbd "C-j") #'windmove-down
      (kbd "C-k") #'windmove-up
      (kbd "C-p") #'comint-previous-input
      (kbd "C-n") #'comint-next-input)))

(defun yxl-evil//setup-evil-text-objects ()
  ;; text objects:
  (define-key evil-outer-text-objects-map "g" #'evil-a-curly)
  (define-key evil-outer-text-objects-map "h" #'evil-a-bracket)
  (define-key evil-outer-text-objects-map "t" #'evil-a-double-quote)
  (define-key evil-outer-text-objects-map "y" #'evil-a-single-quote)
  (define-key evil-inner-text-objects-map "g" #'evil-inner-curly)
  (define-key evil-inner-text-objects-map "h" #'evil-inner-bracket)
  (define-key evil-inner-text-objects-map "t" #'evil-inner-double-quote)
  (define-key evil-inner-text-objects-map "y" #'evil-inner-single-quote)
  (define-key evil-inner-text-objects-map "c" #'yxl-evil-indent-chains)
  (define-key evil-inner-text-objects-map "k" #'evil-textobj-column-word)
  (define-key evil-inner-text-objects-map "K" #'evil-textobj-column-WORD)
  (define-key evil-inner-text-objects-map "q" #'evil-indent-chains))

(defun yxl-evil//setup-evil-eyebrowse ()
  (define-key evil-motion-state-map "gt" #'eyebrowse-next-window-config)
  (define-key evil-motion-state-map "gT" #'eyebrowse-prev-window-config)
  (define-key evil-motion-state-map
    (kbd "C-w C-h") #'eyebrowse-prev-window-config)
  (define-key evil-motion-state-map
    (kbd "C-w C-l") #'eyebrowse-next-window-config)
  ;; Ex commands
  (evil-ex-define-cmd "tabn[ew]" 'eyebrowse-cwc-dired))

(defun yxl-evil//setup-evilified-general ()
  (when (boundp 'evil-evilified-state-map-original)
    (let ((map evil-evilified-state-map-original))
      (define-key map (kbd "o") (kbd "RET"))

      (define-key map "w" #'evil-forward-word-begin)
      (define-key map "e" #'evil-forward-word-end)
      (define-key map "b" #'evil-backward-word-begin)

      (define-key map "W" #'evil-forward-WORD-begin)
      (define-key map "E" #'evil-forward-WORD-end)
      (define-key map "B" #'evil-backward-WORD-begin)

      (define-key map "gg" #'evil-goto-first-line)
      (define-key map "G"  #'evil-goto-line)

      (define-key map "\C-w" 'evil-window-map)

      (define-key map (kbd "zz") #'evil-scroll-line-to-center)
      (define-key map (kbd "zb") #'evil-scroll-line-to-bottom)
      (define-key map (kbd "zt") #'evil-scroll-line-to-top)

      (define-key map "-" #'dired-jump)

      (define-key map "}" 'evil-forward-paragraph)
      (define-key map "{" 'evil-backward-paragraph)

      (define-key map (kbd dotspacemacs-ex-command-key) #'evil-ex)
      (define-key map (kbd ";") #'evil-ex))))

(defun yxl-evil//setup-evilified-c-hjkl ()
  (when (boundp 'evil-evilified-state-map-original)
    (let ((map evil-evilified-state-map-original))
      (define-key map (kbd "C-h") #'evil-window-left)
      (define-key map (kbd "C-j") #'evil-window-down)
      (define-key map (kbd "C-k") #'evil-window-up)
      (define-key map (kbd "C-l") #'evil-window-right))))
