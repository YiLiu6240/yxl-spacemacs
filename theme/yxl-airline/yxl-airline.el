(require 'powerline)
(require 'cl-lib)
(require 'airline-themes)
(require 'yxl-modeline-funcs)

(defun yxl-airline-themes-set-modeline ()
  "Set the airline mode-line-format"
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
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
                                                         (upcase (concat (symbol-name evil-state)
                                                                         (cond (visual-block "-BLOCK")
                                                                               (visual-line "-LINE"))))
                                                       nil))

                          (outer-face
                           (if (powerline-selected-window-active)
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
                           (if (powerline-selected-window-active)
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
                           (if (powerline-selected-window-active)
                               (if (featurep 'evil)
                                   (cond ((eq evil-state (intern "normal")) 'airline-normal-center)
                                         ((eq evil-state (intern "insert")) 'airline-insert-center)
                                         ((eq evil-state (intern "visual")) 'airline-visual-center)
                                         ((eq evil-state (intern "replace")) 'airline-replace-center)
                                         ((eq evil-state (intern "emacs"))   'airline-emacs-center)
                                         (t                                 'airline-normal-center))
                                 'airline-normal-center)
                             'airline-inactive3))

                          ;; Left Hand Side
                          (lhs-mode (if (featurep 'evil)
                                        (list
                                         ;; Evil Mode Name
                                         (powerline-raw (concat " " current-evil-state-string " ") outer-face)
                                         (funcall separator-left outer-face inner-face)
                                         ;; Modified string
                                         (powerline-raw "%*" inner-face 'l))

                                      (list
                                       ;; Modified string
                                       (powerline-raw "%*" outer-face 'l)
                                       ;; Separator >
                                       (powerline-raw " " outer-face)
                                       (funcall separator-left outer-face inner-face))))

                          (lhs-rest (list
                                     ;; ;; Separator >
                                     ;; (powerline-raw (char-to-string #x2b81) inner-face 'l)

                                     ;; Eyebrowse current tab/window config
                                     (if (featurep 'eyebrowse)
                                         (powerline-raw (concat " " (eyebrowse-mode-line-indicator)) inner-face))

                                     ;; Git Branch
                                     (powerline-raw (airline-get-vc) inner-face)

                                     ;; Separator >
                                     (powerline-raw " " inner-face)
                                     (funcall separator-left inner-face center-face)

                                     ;; Directory
                                     (when (eq airline-display-directory 'airline-directory-shortened)
                                       (powerline-raw (airline-shorten-directory default-directory airline-shortened-directory-length) center-face 'l))
                                     (when (eq airline-display-directory 'airline-directory-full)
                                       (powerline-raw default-directory center-face 'l))
                                     (when (eq airline-display-directory nil)
                                       (powerline-raw " " center-face))

                                     ;; Buffer ID
                                     ;; (powerline-buffer-id center-face)
                                     (powerline-raw "%b" center-face)

                                     ;; Current Function (which-function-mode)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       ;; (powerline-raw which-func-format 'l nil))
                                       (powerline-raw which-func-format center-face 'l))

                                     ;; ;; Separator >
                                     ;; (powerline-raw " " center-face)
                                     ;; (funcall separator-left mode-line face1)

                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object center-face 'l))))

                                     ;; ;; Separator <
                                     ;; (powerline-raw " " face1)
                                     ;; (funcall separator-right face1 face2)


                          (lhs (append lhs-mode lhs-rest))

                          ;; Right Hand Side
                          (rhs (list (powerline-raw global-mode-string center-face 'r)

                                     ;; ;; Separator <
                                     ;; (powerline-raw (char-to-string #x2b83) center-face 'l)

                                     ;; Subseparator <
                                     (powerline-raw (char-to-string airline-utf-glyph-subseparator-right) center-face 'l)

                                     ;; Major Mode
                                     (powerline-major-mode center-face 'l)
                                     (powerline-process center-face)

                                     ;; Separator <
                                     (powerline-raw " " center-face)
                                     (funcall separator-right center-face inner-face)

                                     ;; Buffer Size
                                     (when powerline-display-buffer-size
                                       (powerline-buffer-size inner-face 'l))

                                     ;; Mule Info
                                     (when powerline-display-mule-info
                                       (powerline-raw mode-line-mule-info inner-face 'l))

                                     (powerline-raw " " inner-face)

                                     ;; Separator <
                                     (funcall separator-right inner-face outer-face)

                                     ;; LN charachter
                                     (powerline-raw (char-to-string airline-utf-glyph-linenumber) outer-face 'l)

                                     ;; Current Line
                                     (powerline-raw "%4l" outer-face 'l)
                                     (powerline-raw ":" outer-face 'l)
                                     ;; Current Column
                                     (powerline-raw "%3c" outer-face 'r))))


                     ;; Combine Left and Right Hand Sides
                     (concat (powerline-render lhs)
                             (powerline-fill center-face (powerline-width rhs))
                             (powerline-render rhs))))))
  (powerline-reset)
  (kill-local-variable 'mode-line-format))

(provide 'yxl-airline)
