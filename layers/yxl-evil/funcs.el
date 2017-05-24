(defun yxl-evil/setup-evil-main ()
  (progn
    ;; insert state:
    (define-key evil-insert-state-map (kbd "C-h") #'backward-delete-char-untabify)
    (define-key evil-insert-state-map (kbd "C-d") #'delete-forward-char)
    (define-key evil-insert-state-map (kbd "C-a") #'beginning-of-line-text)
    (define-key evil-insert-state-map (kbd "C-e") #'end-of-line)
    (define-key evil-insert-state-map (kbd "C-p") #'previous-line)
    (define-key evil-insert-state-map (kbd "C-n") #'next-line)

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

    ;; normal and motion state:
    (define-key evil-normal-state-map (kbd "j") #'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "k") #'evil-previous-visual-line)
    ;; "g" related commands
    ;; mark: repalce with evil-middle-of-visual-line
    (define-key evil-motion-state-map "gm" #'evil-goto-mark)
    (define-key evil-motion-state-map "gt" #'eyebrowse-next-window-config)
    (define-key evil-motion-state-map "gT" #'eyebrowse-prev-window-config)
    (define-key evil-motion-state-map (kbd "C-w C-h") #'eyebrowse-prev-window-config)
    (define-key evil-motion-state-map (kbd "C-w C-l") #'eyebrowse-next-window-config)

    ;; navigation
    ;; ;; put "HML" to "g/HML", use "HL" as "^$"
    ;; (define-key evil-motion-state-map (kbd "g C-h") 'evil-window-top)
    ;; (define-key evil-motion-state-map (kbd "g C-l") 'evil-window-bottom)
    ;; (define-key evil-motion-state-map (kbd "g C-m") 'evil-window-middle)
    ;; (define-key evil-motion-state-map "H" #'evil-first-non-blank)
    ;; (define-key evil-motion-state-map "L" #'evil-end-of-line)
    (define-key evil-motion-state-map (kbd "C-h") #'evil-window-left)
    (define-key evil-motion-state-map (kbd "C-j") #'evil-window-down)
    (define-key evil-motion-state-map (kbd "C-k") #'evil-window-up)
    (define-key evil-motion-state-map (kbd "C-l") #'evil-window-right)
    (define-key evil-motion-state-map "{" 'yxl-evil-backward-paragraph)
    (define-key evil-motion-state-map "}" 'yxl-evil-forward-paragraph)
    ;; evil snipe
    (evil-define-key 'motion evil-snipe-mode-map "gs" 'evil-snipe-s)
    (evil-define-key 'motion evil-snipe-mode-map "gS" 'evil-snipe-S)
    (evil-define-key 'operator evil-snipe-mode-map "gs" 'evil-snipe-s)
    (evil-define-key 'operator evil-snipe-mode-map "gS" 'evil-snipe-S)
    ;; ;; "q" as a leader
    ;; (define-key evil-normal-state-map "q" nil)
    ;; (define-key evil-normal-state-map "qm" #'evil-execute-macro)
    ;; (define-key evil-normal-state-map "qM" #'evil-record-macro)
    ;; (define-key evil-normal-state-map "qq" #'yxl-evil-quit)
    ;; (define-key evil-normal-state-map "q." #'yxl-evil-execute-last-macro)
    ;; ;; (define-key evil-normal-state-map "qQ" #'evil-save-and-close)
    ;; (define-key evil-normal-state-map "qw" #'evil-write)
    ;; (define-key evil-normal-state-map "qW" #'evil-write-all)
    ;; "\" as another leader
    (define-key evil-motion-state-map "\\" nil)
    (define-key evil-motion-state-map "\\\\" #'scratch-pop-sticky)
    (define-key evil-motion-state-map "\\a" #'scratch-pop)
    (define-key evil-motion-state-map "\\t" #'scratch-pop-top)

    ;; ex:
    ;; ":" and ";"
    (define-key evil-motion-state-map (kbd dotspacemacs-ex-command-key) 'evil-ex)
    (if (equal dotspacemacs-ex-command-key ";")
        (progn
          (define-key evil-motion-state-map (kbd ":") 'evil-repeat-find-char)))

    ;; misc:
    ;; (define-key evil-motion-state-map (kbd "C-S-p") #'helm-M-x)
    (define-key evil-motion-state-map (kbd "C-S-p") #'counsel-M-x)
    (define-key evil-normal-state-map (kbd "_") #'projectile-dired)

    ;; from evil-textobj-column
    (define-key evil-inner-text-objects-map "k" #'evil-textobj-column-word)
    (define-key evil-inner-text-objects-map "K" #'evil-textobj-column-WORD)

    ;; from evil-goodies
    (define-key evil-motion-state-map "g<" #'yxl-evil-go-up-indent)
    (define-key evil-inner-text-objects-map "q" #'evil-indent-chains)

    ;; ---- disabled ----
    ;; vim-surround, use "S"
    ;; (define-key 'visual evil-surround-mode-map "s" #'evil-substitute)
    ;; (define-key 'visual evil-surround-mode-map "S" #'evil-surround-region)
    ;; ;; swap colon and semi colon
    ;; (define-key evil-normal-state-map "g:" #'goto-last-change)
    ;; ;; (define-key evil-motion-state-map ":" #'evil-repeat-find-char)

    ;; Ex commands
    (evil-ex-define-cmd "tabn[ew]" 'eyebrowse-cwc-dired)))

(defun yxl-evil/setup-evil-personal ()
  "personal bindings, rely on external yxl functions"
  ;; stay in normal mode when switch to wdired-mode
  (delete 'wdired-mode evil-insert-state-modes))

(defun yxl-evil/setup-evil-misc ()
  (evil-define-key 'insert comint-mode-map
    (kbd "C-j") #'windmove-down
    (kbd "C-k") #'windmove-up
    (kbd "C-p") #'comint-previous-input
    (kbd "C-n") #'comint-next-input)
  (evil-define-key 'normal comint-mode-map
    (kbd "C-j") #'windmove-down
    (kbd "C-k") #'windmove-up
    (kbd "C-p") #'comint-previous-input
    (kbd "C-n") #'comint-next-input))

(defun yxl-evil/evil-surround-pairs ()
  "press viw then press the trigger key"
  (push '(?g . ("{" . "}")) evil-surround-pairs-alist)
  (push '(?h . ("[" . "]")) evil-surround-pairs-alist)
  (push '(?y . ("\"" . "\"")) evil-surround-pairs-alist)
  (push '(?u . ("'" . "'")) evil-surround-pairs-alist)
  (push '(?m . ("\\\(" . "\\\)")) evil-surround-pairs-alist)
  (push '(?M . ("\\\( " . " \\\)")) evil-surround-pairs-alist)
  (push '(?n . ("\\[" . "\\]")) evil-surround-pairs-alist)
  (push '(?N . ("\\[ " . " \\]")) evil-surround-pairs-alist)
  (push '(?k . ("$ " . " $")) evil-surround-pairs-alist)
  (push '(?K . ("$" . "$")) evil-surround-pairs-alist))

(defun yxl-evil/setup-evilified ()
  (when (boundp 'evil-evilified-state-map-original)
    (progn
      (define-key evil-evilified-state-map-original "w" #'evil-forward-word-begin)
      (define-key evil-evilified-state-map-original "e" #'evil-forward-word-end)
      (define-key evil-evilified-state-map-original "b" #'evil-backward-word-begin)

      (define-key evil-evilified-state-map-original "W" #'evil-forward-WORD-begin)
      (define-key evil-evilified-state-map-original "E" #'evil-forward-WORD-end)
      (define-key evil-evilified-state-map-original "B" #'evil-backward-WORD-begin)

      (define-key evil-evilified-state-map-original "gg" #'evil-goto-first-line)
      (define-key evil-evilified-state-map-original "G"  #'evil-goto-line)
      (define-key evil-evilified-state-map-original "gT" #'eyebrowse-prev-window-config)
      (define-key evil-evilified-state-map-original "gt" #'eyebrowse-next-window-config)
      (define-key evil-evilified-state-map-original (kbd "g C-h") #'eyebrowse-prev-window-config)
      (define-key evil-evilified-state-map-original (kbd "g C-l") #'eyebrowse-next-window-config)

      (define-key evil-evilified-state-map-original "\C-w" 'evil-window-map)

      (define-key evil-evilified-state-map-original (kbd "C-h") #'evil-window-left)
      (define-key evil-evilified-state-map-original (kbd "C-j") #'evil-window-down)
      (define-key evil-evilified-state-map-original (kbd "C-k") #'evil-window-up)
      (define-key evil-evilified-state-map-original (kbd "C-l") #'evil-window-right)

      (define-key evil-evilified-state-map-original (kbd "zz") #'evil-scroll-line-to-center)
      (define-key evil-evilified-state-map-original (kbd "zb") #'evil-scroll-line-to-bottom)
      (define-key evil-evilified-state-map-original (kbd "zt") #'evil-scroll-line-to-top)

      (define-key evil-evilified-state-map-original "-" #'dired-jump)

      (define-key evil-evilified-state-map-original
        (kbd dotspacemacs-ex-command-key) #'evil-ex))))

(defun yxl-evil/setup-evilified-personal ()
  (when (boundp 'evil-evilified-state-map-original)
    (progn
      (define-key evil-evilified-state-map-original (kbd "C-a h") #'yxl-frame-select-meta)
      (define-key evil-evilified-state-map-original (kbd "C-a j") #'yxl-frame-select-repl)
      (define-key evil-evilified-state-map-original (kbd "C-a k") #'yxl-frame-select-code)
      (define-key evil-evilified-state-map-original (kbd "C-a l") #'yxl-frame-select-config))))
