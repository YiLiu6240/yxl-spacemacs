(setq yxl-edit-packages '(parinfer
                          lispy
                          smartparens
                          imenu-anywhere
                          hl-todo
                          (hi-lock :location built-in)
                          narrow-indirect
                          langtool
                          flyspell))

(defun yxl-edit/init-parinfer ()
  (use-package parinfer
    :defer t
    :diminish parinfer-mode
    :init
    (progn
      (add-hook 'emacs-lisp-mode-hook 'parinfer-mode)
      (add-hook 'clojure-mode-hook 'parinfer-mode)
      (add-hook 'common-lisp-mode-hook 'parinfer-mode)
      (add-hook 'scheme-mode-hook 'parinfer-mode)
      (add-hook 'lisp-mode-hook 'parinfer-mode)
      (spacemacs|add-toggle parinfer-indent
        :evil-leader "tP"
        :documentation "Enable Parinfer Indent Mode."
        :if (bound-and-true-p parinfer-mode)
        :status (eq parinfer--mode 'indent)
        :on (parinfer-toggle-mode)
        :off (parinfer-toggle-mode))
      (setq parinfer-extensions '(defaults pretty-parens evil lispy smart-tab smart-yank)))))

(defun yxl-edit/init-lispy ()
  (use-package lispy
    :defer t
    :config
    (progn
      (define-key emacs-lisp-mode-map
        (kbd "C-<tab>") 'lispy-indent-adjust-parens)
      (define-key emacs-lisp-mode-map
        (kbd "C-S-<tab>") 'lispy-dedent-adjust-parens)
      (define-key emacs-lisp-mode-map
        (kbd "<C-iso-lefttab>") 'lispy-dedent-adjust-parens)
      (define-key lispy-mode-map (kbd "C-2") 'lispy-describe-inline)
      (define-key lispy-mode-map (kbd "C-3") 'lispy-arglist-inline))))
      ;; (define-key lispy-mode-map (kbd "C-k") 'lispy-splice)
      ;; (define-key lispy-mode-map (kbd "C-3") 'lispy-mark-symbol)

(defun yxl-edit/pre-init-smartparens ()
  (spacemacs|use-package-add-hook smartparens
    :post-config
    (progn
      ;; https://www.reddit.com/r/emacs/comments/54agp2/from_an_evil_perspective_how_to_efficiently_edit/
      ;; (define-key sp-keymap (kbd "C-<tab>") 'sp-indent-adjust-sexp)
      ;; (define-key sp-keymap (kbd "C-S-<iso-lefttab>") 'sp-dedent-adjust-sexp)
      (setq sp-highlight-pair-overlay nil)
      (setq sp-highlight-wrap-overlay nil)
      (setq sp-highlight-wrap-tag-overlay nil))))

(defun yxl-edit/post-init-hl-todo ()
  (with-eval-after-load 'hl-todo
    (setq-default hl-todo-keyword-faces
                  `(("HOLD" . (:weight bold :foreground ,(face-attribute 'font-lock-function-name-face :foreground)))
                    ("TODAY" . (:weight bold :foreground ,(face-attribute 'font-lock-warning-face :foreground)))
                    ("TODO" . (:weight bold :foreground ,(face-attribute 'font-lock-variable-name-face :foreground)))
                    ("NEXT" . (:weight bold :foreground ,(face-attribute 'font-lock-constant-face :foreground)))
                    ("PROG" . (:weight bold :foreground ,(face-attribute 'font-lock-function-name-face :foreground)))
                    ("WIP" . (:weight bold :foreground ,(face-attribute 'font-lock-function-name-face :foreground)))
                    ("OKAY" . (:weight bold :foreground ,(face-attribute 'success :foreground)))
                    ("FOLLOW" . (:weight bold :foreground ,(face-attribute 'font-lock-function-name-face :foreground)))
                    ("DONT" . (:weight bold :foreground ,(face-attribute 'font-lock-builtin-face :foreground)))
                    ("IDEA" . (:weight bold :foreground ,(face-attribute 'font-lock-builtin-face :foreground)))
                    ("DOC" . (:weight bold :foreground ,(face-attribute 'font-lock-builtin-face :foreground)))
                    ("ISSUE" . (:weight bold :foreground ,(face-attribute 'font-lock-warning-face :foreground)))
                    ("FAIL" . (:weight bold :foreground ,(face-attribute 'font-lock-warning-face :foreground)))
                    ("DONE" . (:weight bold :foreground ,(face-attribute 'font-lock-comment-face :foreground)))
                    ("NOTE" . (:weight bold :foreground ,(face-attribute 'font-lock-string-face :foreground)))
                    ("REVIEW" . (:weight bold :foreground ,(face-attribute 'font-lock-string-face :foreground)))
                    ("HACK" . (:weight bold :foreground ,(face-attribute 'font-lock-function-name-face :foreground)))
                    ("PATCH" . (:weight bold :foreground ,(face-attribute 'font-lock-function-name-face :foreground)))
                    ("FIXME" . (:weight bold :foreground ,(face-attribute 'font-lock-variable-name-face :foreground)))
                    ("XXX" . (:weight bold :foreground ,(face-attribute 'font-lock-variable-name-face :foreground)))
                    ("???" . (:weight bold :foreground ,(face-attribute 'font-lock-warning-face :foreground)))))))

(defun yxl-edit/post-init-hi-lock ()
  (with-eval-after-load 'hi-lock
    (progn
      (setq hi-lock-auto-select-face t)
      (setq hi-lock-face-defaults '("hi-blue" "hi-green" "hi-yellow" "hi-pink")))))

(defun yxl-edit/init-imenu-anywhere ()
  (use-package imenu-anywhere
    :defer t
    :commands (imenu-anywhere yxl-imenu-anywhere ivy-imenu-anywhere)
    :config
    (progn
      (defun imenu-anywhere-buffer-visible-p (current other)
        (let ((visible-buffer-list (delq nil
                                         (mapcar
                                          (lambda (buffer)
                                            (when (get-buffer-window buffer)
                                              buffer))
                                          (buffer-list)))))
          (member other visible-buffer-list)))
      ;; remove same-project-p, too confusing
      ;; TODO: write a function to predicate files in a filter list
      (setq imenu-anywhere-buffer-filter-functions '(imenu-anywhere-same-mode-p
                                                     imenu-anywhere-friendly-mode-p))
      (defun yxl-imenu-anywhere ()
        (interactive)
        (if current-prefix-arg
            (let ((imenu-anywhere-buffer-filter-functions '(imenu-anywhere-buffer-visible-p)))
              (ivy-imenu-anywhere))
          (ivy-imenu-anywhere))))))

(defun yxl-edit/post-init-flyspell ()
  (with-eval-after-load 'flyspell
    ;; hunspell
    ;; download dictionary from http://wordlist.aspell.net/dicts/
    (when (executable-find "hunspell")
      (progn
        (setq-default ispell-program-name "hunspell")
        (cond ((eq system-type 'darwin)
               (progn
                 (setenv "DICPATH"
                         (concat (getenv "HOME") "/Library/Spelling"))
                 (setenv "DICTIONARY" "en_GB-large")))
              ;; self-defined in ubuntu: ~/.local/share/hunspell
              ((eq system-type 'gnu/linux)
               (progn
                 (setenv "DICPATH"
                         (concat (getenv "HOME") "/.local/share/hunspell"))))
              (t nil))
        (setq-default ispell-local-dictionary "en_GB-large")))
    (define-key flyspell-mouse-map (kbd "<C-down-mouse-1>") #'flyspell-correct-word)
    (define-key flyspell-mouse-map (kbd "<C-mouse-1>") 'undefined)))

(defun yxl-edit/init-langtool ()
  (use-package langtool
    :commands (langtool-hydra/body langtool-check)
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys
        "al" #'langtool-hydra/body))
    :config
    (progn
      ;; TODO: get path done for all platforms
      (setq langtool-language-tool-jar
            (cond (;; windows:
                   (eq system-type 'windows-nt)
                   nil)
                  ;; macOS: brew install languagetool
                  ((eq system-type 'darwin)
                   "/usr/local/Cellar/languagetool/3.6/libexec/languagetool-commandline.jar")
                  ;; linux: download standalone and choose a destination
                  (t (expand-file-name
                      "~/.local/share/LanguageTool-3.6/languagetool-commandline.jar"))))
      (defun langtool-autoshow-detail-popup (overlays)
        (when (require 'popup nil t)
          ;; Do not interrupt current popup
          (unless (or popup-instances
                      ;; suppress popup after type `C-g` .
                      (memq last-command '(keyboard-quit)))
            (let ((msg (langtool-details-error-message overlays)))
              (popup-tip msg)))))
      (setq langtool-autoshow-message-function
            'langtool-autoshow-detail-popup)
      (defhydra langtool-hydra (:color blue :hint nil :columns 4)
        ("c" langtool-check "langtool-check")
        ("d" langtool-check-done "langtool-check-done")
        ("s" langtool-switch-default-language "langtool-switch-default-language")
        ("m" langtool-show-message-at-point "langtool-show-message-at-point")
        ("b" langtool-correct-buffer "langtool-correct-buffer")))))

(defun yxl-edit/init-narrow-indirect ()
  (use-package narrow-indirect
    :defer t
    :commands (ni-narrow-to-region-indirect-same-window)
    :config
    (progn
      (defun ni-narrow-to-region-indirect-same-window (start end here &optional full-name text msgp)
        (interactive
         (list (region-beginning) (region-end) (point) (and current-prefix-arg  (read-string "Buffer name: ")) nil 'MSGP))
        (if (and (= start end)  msgp)
            (message "Region is empty")
          (deactivate-mark)
          (let* ((buf  (or full-name  text  (ni-buffer-substring-collapsed-visible start end)))
                 (buf  (or full-name  (concat ni-buf-name-prefix (buffer-name) ni-buf-name-separator buf)))
                 (buf  (or full-name  (substring buf 0 (min (length buf) ni-narrowed-buf-name-max))))
                 (buf   (clone-indirect-buffer buf nil)))
            (with-current-buffer buf (narrow-to-region start end) (goto-char here))
            (switch-to-buffer buf)
            (setq mode-line-buffer-identification  (list (propertize (car mode-line-buffer-identification)
                                                                     'face 'ni-mode-line-buffer-id)))))))))
