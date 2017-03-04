(setq yxl-ui-packages '(
                        ;; spaceline
                        ;; (yxl-spaceline :toggle (bound-and-true-p spaceline))
                        ;; (yxl-mode-line :location local
                        ;;                :toggle (not (bound-and-true-p spaceline)))
                        powerline
                        (vim-powerline :location local)
                        (mode-line-timer :location local)))
                        airline-themes))

;; (defun yxl-ui/init-yxl-mode-line ()
;;   (use-package yxl-mode-line))

;; (defun yxl-ui/init-yxl-spaceline ()
;;   (use-package yxl-spaceline))
(defun yxl-ui/init-powerline ())

(defun yxl-ui/init-vim-powerline ()
  (require 'powerline)

  ;; HACK: always use 'nil (a pipe) as separator
  (setq powerline-default-separator nil)
  (defun powerline-current-separator ()
    "Get the current default separator. HACK: return 'nil in CLI."
    (if window-system
        powerline-default-separator
      'nil))
  (defun powerline-raw (str &optional face pad)
    "Render STR as mode-line data using FACE and optionally
PAD import on left (l) or right (r) or left-right (lr)."
    (when str
      (let* ((rendered-str (format-mode-line str))
             (padded-str (concat
                          (when (and (> (length rendered-str) 0)
                                     (or (eq pad 'l) (eq pad 'lr))) " ")
                          (if (listp str) rendered-str str)
                          (when (and (> (length rendered-str) 0)
                                     (or (eq pad 'r) (eq pad 'lr))) " "))))
        (if face
            (pl/add-text-property padded-str 'face face)
          padded-str))))

  (require 'vim-powerline-theme)
  (powerline-vimish-theme)

  (defun spacemacs//set-vimish-powerline-for-startup-buffers ()
    "Set the powerline for buffers created when Emacs starts."
    (dolist (buffer '("*Messages*" "*spacemacs*" "*Compile-Log*"))
      (when (get-buffer buffer)
        (with-current-buffer buffer
          (setq-local mode-line-format (default-value 'mode-line-format))
          (powerline-set-selected-window)
          (powerline-reset)))))
  (add-hook 'emacs-startup-hook
            'spacemacs//set-vimish-powerline-for-startup-buffers))

(defun yxl-ui/init-mode-line-timer ()
  (use-package mode-line-timer
    :defer t
    :commands (mode-line-timer-start)))
(defun yxl-ui/init-airline-themes ()
  (use-package airline-themes
    :ensure t
    :config
    (progn
      (setq airline-cursor-colors nil)
      (setq powerline-utf-8-separator-left        #xe0b0
            powerline-utf-8-separator-right       #xe0b2
            airline-utf-glyph-separator-left      #xe0b0
            airline-utf-glyph-separator-right     #xe0b2
            airline-utf-glyph-subseparator-left   #xe0b1
            airline-utf-glyph-subseparator-right  #xe0b3
            airline-utf-glyph-branch              #xe0a0
            airline-utf-glyph-readonly            #xe0a2
            airline-utf-glyph-linenumber          #xe0a1))))
