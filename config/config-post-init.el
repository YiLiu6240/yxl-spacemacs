;; --------
;; general
;; --------
;; use transparent/system background in terminal
(if (not (display-graphic-p))
    (set-background-color "invalid"))
;; emacs general
;; osx
(setq-default mac-option-modifier 'super
              mac-command-modifier 'meta)
(setq-default ns-use-srgb-colorspace t)
(setq-default initial-major-mode 'markdown-mode)
;; ui
(setq-default neo-theme 'ascii)
(setq-default fci-rule-color (face-attribute 'highlight :background))

;; evil escape
(setq-default evil-escape-key-sequence "jk")
;; only use "jk" in insert state
(setq-default evil-escape-excluded-states '(visual
                                            evilified
                                            normal
                                            motion
                                            emacs
                                            replace
                                            hybrid
                                            lisp
                                            iedit
                                            iedit-insert))
(setq-default evil-escape-delay 1)
(setq-default evil-escape-excluded-major-modes '(magit-mode))

;; yas
(setq-default yas-snippet-dirs (expand-file-name "snippets" dotspacemacs-directory))

;; indent
(setq-default tab-width 4)
(setq-default evil-shift-width 4)
(setq-default indent-tabs-mode nil)

;; mode list
(add-to-list 'auto-mode-alist '("\\.todo$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.inbox$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.tex$" . latex-mode))
(add-to-list 'auto-mode-alist '("\\.sublime-settings$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.sublime-keymap$" . json-mode))

;; misc stuff
(setq-default require-final-newline t)
(setq-default auto-revert-interval 60)

;; --------
;; display-buffer-alist
;; --------
;; shell: display in current window
(add-to-list 'display-buffer-alist '("\\`\\*e?shell" display-buffer-same-window))
(add-to-list 'display-buffer-alist '("\\*R" display-buffer-same-window))
;; FIXME: not working
(add-to-list 'display-buffer-alist '("\\*Python\\*" display-buffer-same-window))
(add-to-list 'display-buffer-alist '("\\*magit:" display-buffer-same-window))
(add-to-list 'display-buffer-alist '("\\*PDF" display-buffer-at-bottom))


;; --------
;; yas
;; --------
;; solve an known issue revolving evil visual and yasnippet
;; TODO: check if actually need that hook
(with-eval-after-load 'yasnippet
  (add-hook 'yas-before-expand-snippet-hook
            #'(lambda()
                (when (evil-visual-state-p)
                  (let ((p (point))
                        (m (mark)))
                    (evil-insert-state)
                    (goto-char p)
                    (set-mark m))))))

;; ;; deft
;; (setq deft-extensions '("md" "txt" "org"))
;; (setq deft-directory "~/Dropbox/notes")

;; spacemacs buffer
(add-hook 'spacemacs-buffer-mode-hook
          (lambda () (define-key spacemacs-buffer-mode-map
                       "o" 'widget-button-press)))

;; --------
;; proselint
;; --------
(with-eval-after-load 'flycheck
  (flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message (one-or-more not-newline)
                       (zero-or-more "\n" (any " ") (one-or-more not-newline)))
              line-end))
    :modes (text-mode markdown-mode gfm-mode LaTeX-mode))
  (add-to-list 'flycheck-checkers 'proselint))

;; --------
;; under REVIEW
;; --------

;; (add-hook 'lua-mode-hook (lambda () (setq-local origami-fold-style 'triple-braces)))
(add-hook 'bibtex-mode-hook (lambda () (setq-local origami-fold-style 'triple-braces)))
(setq-default origami-show-fold-header nil)

;; --------
;; misc configs
;; --------
(setq-default bookmark-default-file "~/Dropbox/inbox/helm-bookmark")

;; tramp bug, from zilongshanren
(setq-default tramp-ssh-controlmaster-options
              "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

;; deal with html export problem in orgmode with fci
;; https://github.com/alpaker/Fill-Column-Indicator/issues/45
(defun fci-mode-override-advice (&rest args))
(advice-add 'org-html-fontify-code :around
            (lambda (fun &rest args)
              (advice-add 'fci-mode :override #'fci-mode-override-advice)
              (let ((result  (apply fun args)))
                (advice-remove 'fci-mode #'fci-mode-override-advice)
                result)))
