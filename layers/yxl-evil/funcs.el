(defun yxl-evil/setup-evil ()
  (progn
   ;; insert state:
   (define-key evil-insert-state-map (kbd "C-h") #'backward-delete-char-untabify)
   (define-key evil-insert-state-map (kbd "C-d") #'delete-forward-char)
   (define-key evil-insert-state-map (kbd "C-a") #'beginning-of-line-text)
   (define-key evil-insert-state-map (kbd "C-e") #'end-of-line)
   (define-key evil-insert-state-map (kbd "C-p") #'previous-line)
   (define-key evil-insert-state-map (kbd "C-n") #'next-line)

   ;; text objects:
   (define-key evil-outer-text-objects-map "g" 'evil-a-curly)
   (define-key evil-outer-text-objects-map "h" 'evil-a-bracket)
   (define-key evil-inner-text-objects-map "g" 'evil-inner-curly)
   (define-key evil-inner-text-objects-map "h" 'evil-inner-bracket)

   ;; normal and motion state:
   (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
   (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
   ;; "g" related commands
   ;; mark: repalce with evil-middle-of-visual-line
   (define-key evil-motion-state-map "gm" #'evil-goto-mark)
   (define-key evil-motion-state-map "gt" #'eyebrowse-next-window-config)
   (define-key evil-motion-state-map "gT" #'eyebrowse-prev-window-config)
   (define-key evil-motion-state-map "gH" #'evil-first-non-blank)
   (define-key evil-motion-state-map "gL" #'evil-end-of-line)
   ;; navigation
   (define-key evil-motion-state-map "H" #'eyebrowse-prev-window-config)
   (define-key evil-motion-state-map "L" #'eyebrowse-next-window-config)
   (define-key evil-motion-state-map (kbd "C-h") #'evil-window-left)
   (define-key evil-motion-state-map (kbd "C-j") #'evil-window-down)
   (define-key evil-motion-state-map (kbd "C-k") #'evil-window-up)
   (define-key evil-motion-state-map (kbd "C-l") #'evil-window-right)
   ;; "q" as a leader
   (define-key evil-normal-state-map "q" nil)
   (define-key evil-normal-state-map "qm" #'evil-execute-macro)
   (define-key evil-normal-state-map "qM" #'evil-record-macro)
   (define-key evil-normal-state-map "qq" #'evil-quit)
   (define-key evil-normal-state-map "qQ" #'evil-save-and-close)
   (define-key evil-normal-state-map "qw" #'evil-write)
   (define-key evil-normal-state-map "qW" #'evil-write-all)
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
   ;; find comma in sentence TODO: let them find period as well
   (define-key evil-motion-state-map (kbd "C-)") 'evil-sentence-comma-forward)
   (define-key evil-motion-state-map (kbd "C-(") 'evil-sentence-comma-backward)
   ;; (define-key evil-motion-state-map (kbd "C-S-p") #'helm-M-x)
   (define-key evil-motion-state-map (kbd "C-S-p") #'counsel-M-x)
   (define-key evil-normal-state-map (kbd "_") #'projectile-dired)
   ;; ---- disabled ----
   ;; vim-surround, use "S"
   ;; (define-key 'visual evil-surround-mode-map "s" #'evil-substitute)
   ;; (define-key 'visual evil-surround-mode-map "S" #'evil-surround-region)
   ;; ;; swap colon and semi colon
   ;; (define-key evil-normal-state-map "g:" #'goto-last-change)
   ;; ;; (define-key evil-motion-state-map ":" #'evil-repeat-find-char)
   ))

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
  (push '(?q . ("\"" . "\"")) evil-surround-pairs-alist)
  (push '(?w . ("'" . "'")) evil-surround-pairs-alist)
  (push '(?m . ("\\\(" . "\\\)")) evil-surround-pairs-alist)
  (push '(?M . ("\\\( " . " \\\)")) evil-surround-pairs-alist)
  (push '(?n . ("\\[" . "\\]")) evil-surround-pairs-alist)
  (push '(?N . ("\\[ " . " \\]")) evil-surround-pairs-alist))

(defun yxl-evil/setup-evilified ()
  (when (boundp 'evil-evilified-state-map-original)
    (progn
      (define-key evil-evilified-state-map-original "gg" #'evil-goto-first-line)
      (define-key evil-evilified-state-map-original "G"  #'evil-goto-line)
      (define-key evil-evilified-state-map-original "gT" #'eyebrowse-prev-window-config)
      (define-key evil-evilified-state-map-original "gt" #'eyebrowse-next-window-config)
      (define-key evil-evilified-state-map-original
        (kbd "H") #'eyebrowse-prev-window-config)
      (define-key evil-evilified-state-map-original
        (kbd "L") #'eyebrowse-next-window-config)
      (define-key evil-evilified-state-map-original (kbd "C-h") #'evil-window-left)
      (define-key evil-evilified-state-map-original (kbd "C-j") #'evil-window-down)
      (define-key evil-evilified-state-map-original (kbd "C-k") #'evil-window-up)
      (define-key evil-evilified-state-map-original (kbd "C-l") #'evil-window-right)
      (define-key evil-evilified-state-map-original
        (kbd dotspacemacs-ex-command-key) #'evil-ex))))
