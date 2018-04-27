(require 'powerline)
(require 'cl-lib)
(require 'airline-themes)
(require 'yxl-modeline-funcs)

(defun yxl-airline-themes-set-deftheme (theme-name)
  "Set appropriate face attributes"

  (when airline-eshell-colors
    (airline-themes-set-eshell-prompt))

  ;; do not attempt to override helm color
  ;; (when airline-helm-colors
  ;;   (custom-theme-set-faces
  ;;    theme-name
  ;;    `(helm-header           ((t ( :foreground ,insert-inner-foreground  :background ,insert-inner-background  :bold t))))
  ;;    `(helm-selection        ((t ( :foreground ,insert-outer-foreground  :background ,insert-outer-background  :bold t))))
  ;;    `(helm-source-header    ((t ( :foreground ,insert-center-foreground :background ,insert-center-background :bold t))))
  ;;    `(helm-candidate-number ((t ( :foreground ,normal-inner-foreground  :background ,normal-inner-background  :bold t))))
  ;;    `(helm-selection-line   ((t ( :foreground ,normal-center-foreground :background ,normal-center-background :bold t))))))

  (custom-theme-set-faces
   theme-name
   `(which-func            ((t ( :foreground ,normal-center-foreground :background ,normal-center-background :bold t))))

   `(airline-normal-outer  ((t ( :foreground ,normal-outer-foreground  :background ,normal-outer-background :bold t))))
   `(airline-normal-inner  ((t ( :foreground ,normal-inner-foreground  :background ,normal-inner-background :bold t))))
   `(airline-normal-center ((t ( :foreground ,normal-center-foreground :background ,normal-center-background))))

   `(airline-insert-outer  ((t ( :foreground ,insert-outer-foreground  :background ,insert-outer-background :bold t))))
   `(airline-insert-inner  ((t ( :foreground ,insert-inner-foreground  :background ,insert-inner-background :bold t))))
   `(airline-insert-center ((t ( :foreground ,insert-center-foreground :background ,insert-center-background))))

   `(airline-visual-outer  ((t ( :foreground ,visual-outer-foreground  :background ,visual-outer-background :bold t))))
   `(airline-visual-inner  ((t ( :foreground ,visual-inner-foreground  :background ,visual-inner-background :bold t))))
   `(airline-visual-center ((t ( :foreground ,visual-center-foreground :background ,visual-center-background))))

   `(airline-replace-outer ((t ( :foreground ,replace-outer-foreground :background ,replace-outer-background :bold t))))
   `(airline-replace-inner  ((t ( :foreground ,replace-inner-foreground  :background ,replace-inner-background :bold t))))
   `(airline-replace-center ((t ( :foreground ,replace-center-foreground :background ,replace-center-background))))

   `(airline-emacs-outer   ((t ( :foreground ,emacs-outer-foreground   :background ,emacs-outer-background :bold t))))
   `(airline-emacs-inner  ((t ( :foreground ,emacs-inner-foreground  :background ,emacs-inner-background :bold t))))
   `(airline-emacs-center ((t ( :foreground ,emacs-center-foreground :background ,emacs-center-background))))

   `(powerline-inactive1   ((t ( :foreground ,inactive1-foreground     :background ,inactive1-background :bold t))))
   `(powerline-inactive2   ((t ( :foreground ,inactive2-foreground     :background ,inactive2-background :bold t))))
   `(airline-inactive3   ((t ( :foreground ,inactive3-foreground     :background ,inactive3-background))))

   `(mode-line             ((t ( :foreground ,normal-center-foreground :background ,normal-center-background :box (:color ,color-border :line-width 1) :underline nil :overline nil))))
   `(mode-line-inactive    ((t ( :foreground ,inactive1-foreground     :background ,inactive1-background     :box (:color ,color-border :line-width 1) :underline nil :overline nil))))
   `(mode-line-buffer-id   ((t ( :foreground ,normal-outer-foreground  :background ,normal-outer-background  :box (:color ,color-border :line-width 1) :underline nil :overline nil))))))

(defun yxl-airline-themes-set-modeline ()
  "Set the airline mode-line-format"
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line-face (if active 'mode-line 'mode-line-inactive))
                          (visual-block (if (featurep 'evil)
                                            (and (evil-visual-state-p)
                                                 (eq evil-visual-selection 'block))
                                          nil))
                          (visual-line (if (featurep 'evil)
                                           (and (evil-visual-state-p)
                                                (eq evil-visual-selection 'line))
                                         nil))
                          (current-evil-state-string (if (featurep 'evil)
                                                         ;; (upcase (concat (symbol-name evil-state)
                                                         ;;                 (cond (visual-block "-BLOCK")
                                                         ;;                       (visual-line "-LINE"))))
                                                       (modeline-get-state-symbol (format "%s" evil-state))
                                                       nil))

                          (outer-face
                           (if active
                               (if (featurep 'evil)
                                   (cond ((eq evil-state (intern "normal"))  'airline-normal-outer)
                                         ((eq evil-state (intern "insert"))  'airline-insert-outer)
                                         ((eq evil-state (intern "visual"))  'airline-visual-outer)
                                         ((eq evil-state (intern "replace")) 'airline-replace-outer)
                                         ((eq evil-state (intern "emacs"))   'airline-emacs-outer)
                                         (t                                  'airline-normal-outer))
                                 'airline-normal-outer)
                             'powerline-inactive1))

                          (inner-face
                           (if active
                               (if (featurep 'evil)
                                   (cond ((eq evil-state (intern "normal")) 'airline-normal-inner)
                                         ((eq evil-state (intern "insert")) 'airline-insert-inner)
                                         ((eq evil-state (intern "visual")) 'airline-visual-inner)
                                         ((eq evil-state (intern "replace")) 'airline-replace-inner)
                                         ((eq evil-state (intern "emacs"))   'airline-emacs-inner)
                                         (t                                 'airline-normal-inner))
                                 'airline-normal-inner)
                             'powerline-inactive2))

                          (center-face
                           (if active
                               (if (featurep 'evil)
                                   (cond ((eq evil-state (intern "normal")) 'airline-normal-center)
                                         ((eq evil-state (intern "insert")) 'airline-insert-center)
                                         ((eq evil-state (intern "visual")) 'airline-visual-center)
                                         ((eq evil-state (intern "replace")) 'airline-replace-center)
                                         ((eq evil-state (intern "emacs"))   'airline-emacs-center)
                                         (t                                 'airline-normal-center))
                                 'airline-normal-center)
                             'airline-inactive3))

                          (pl-sep (powerline-raw " " 'font-lock-comment-face 'lr))

                          ;; Left Hand Side
                          (lhs-mode (list
                                     ;; ;; Evil Mode Name
                                     ;; (powerline-raw (concat " " current-evil-state-string " ") outer-face)
                                     ;; winum, otherwise editor state
                                     (if (bound-and-true-p winum-mode)
                                         (powerline-raw (format " %s " (winum-get-number)) outer-face)
                                       (powerline-raw (format " %s " current-evil-state-string) outer-face))
                                     ;; Buffer ID
                                     (powerline-raw (concat " " (buffer-id-short 100)) inner-face 'lr)
                                     ;; Modified string
                                     (powerline-raw " %* " inner-face 'lr)
                                     (powerline-raw (modeline-window-dedication) inner-face 'lr)))

                          (lhs-rest (list
                                     (powerline-raw " " center-face 'lr)
                                     ;; magit
                                     (when (featurep 'magit)
                                       (powerline-raw (concat pl-sep (magit-get-current-branch) pl-sep) center-face 'lr))
                                     ;; selection info
                                     (when (or mark-active
                                               (and (bound-and-true-p evil-local-mode)
                                                    (eq 'visual evil-state)))
                                       (powerline-raw (concat " " (modeline-selection-info) " " pl-sep) center-face 'lr))
                                     ;; anzu
                                     (when (bound-and-true-p anzu--state)
                                       (powerline-raw (concat " " (anzu--update-mode-line) " " pl-sep) center-face 'lr))
                                     ;; pdf pages
                                     (when (eq 'pdf-view-mode major-mode)
                                       (powerline-raw (concat " " (modeline-pdfview-page-number) " " pl-sep) center-face 'lr))
                                     ;; flycheck
                                     (when (bound-and-true-p flycheck-mode)
                                       (powerline-raw (concat " " (modeline-flycheck) " ") center-face 'lr))))

                          (lhs (append lhs-mode lhs-rest))

                          ;; Right Hand Side
                          (rhs (list (powerline-raw global-mode-string center-face 'r)
                                     ;; erc
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object center-face 'r))
                                     ;; Current Function (which-function-mode)
                                     (when (and (boundp 'which-function-mode) which-function-mode)
                                       (powerline-raw which-func-format center-face 'l))
                                     (powerline-raw " " center-face 'lr)
                                     ;; Eyebrowse current tab/window config
                                     (when (and active (boundp 'eyebrowse-mode))
                                       (powerline-raw (concat " " (eyebrowse-mode-line-indicator) " ") center-face 'lr))
                                     ;; process
                                     (powerline-process center-face 'lr)
                                     (powerline-raw (concat pl-sep (modeline-font-frame-scales) pl-sep) center-face 'lr)
                                     (powerline-raw (concat pl-sep (modeline-buffer-encoding) pl-sep) center-face 'lr)
                                     ;; Major Mode
                                     (powerline-raw " " inner-face 'lr)
                                     (powerline-major-mode inner-face 'lr)
                                     (powerline-raw " " inner-face 'lr)
                                     ;; lin num and total linum
                                     (powerline-raw "%l" outer-face 'l)
                                     (powerline-raw "/" outer-face)
                                     (powerline-raw (format "%s" (line-number-at-pos (point-max)))
                                                    outer-face 'r))))

                     ;; Combine Left and Right Hand Sides
                     (concat (powerline-render lhs)
                             (powerline-fill center-face (powerline-width rhs))
                             (powerline-render rhs))))))
  (powerline-reset)
  (kill-local-variable 'mode-line-format))

(provide 'yxl-airline)
