;; mac: use super+3 as "#"
(global-set-key (kbd "s-3") (lambda () (interactive) (insert "#")))
;; (global-set-key (kbd "C-S-p") #'helm-M-x)
(global-set-key (kbd "C-S-p") #'counsel-M-x)
(global-set-key (kbd "C-S-o") #'company-yasnippet)
(global-set-key (kbd "C-S-y") #'yas-insert-snippet)
(global-set-key (kbd "C-h") #'delete-backward-char)

(global-set-key (kbd "M--") (lambda () (interactive) (insert "<-")))
(global-set-key (kbd "C-M--") (lambda () (interactive) (insert "=>")))

(defun yas-or-company ()
  (interactive)
  (if company-mode
      (call-interactively #'company-yasnippet)
    (yas-insert-snippet)))

(global-set-key (kbd "M-<SPC>") #'yas-or-company)
(define-key isearch-mode-map "\C-h" #'isearch-delete-char)

;; overwrite stock bindings
(spacemacs/set-leader-keys
  ;; workaround
  "," #'eval-expression
  "'" #'yxl-shell-invoke
  "<SPC>" #'yxl-hydra-ace-window/body
  "aa" #'yxl-invoke-applications
  "bB" #'yxl-buffer-switch-same-major-mode
  "bb" #'yxl-buffer-switch
  "bh" #'yxl-utils/home
  ;; TODO: rm this with next spacemacs update
  "bM" #'view-echo-area-messages
  "bm" #'yxl-buffer-switch-same-major-mode
  "bx" #'kill-buffer-and-window
  "ff" #'yxl-find-file-counsel
  "fF" #'find-file-in-current-directory
  "fY" #'yxl-show-and-copy-buffer-filename-in-projectile
  "fO" #'spacemacs/open-file-or-directory-in-external-app ; with C-u open in desktop
  "fo" #'yxl-open-file-external
  "fp" #'counsel-projectile-find-file
  "fP" #'find-file-in-project-truename
  "bY" #'yxl-buffer-store-name
  "bP" #'yxl-buffer-visit-stored-buffer
  "dd" #'yxl-dired-ivy-switch-buffer
  "hdF" #'counsel-faces
  "i <SPC>" #'yxl-evil-insert-newline-around
  "ii" #'yxl-evil-insert-space
  "ia" #'yxl-evil-apend-space
  "jj" #'evil-avy-goto-char-2
  "l" #'spacemacs/workspaces-transient-state/body
  "LY" #'yxl-workspace/record-config
  "LP" #'yxl-workspace/load-config
  "nr" #'ni-narrow-to-region-indirect-same-window
  "ph" #'yxl-project-helm
  "ps" #'yxl-project-shell-popup
  "p'" nil
  "pc" #'yxl-project-cite
  "pC" #'projectile-compile-project
  "pO" #'yxl-project-select
  "po" #'yxl-project-popup
  "pm" #'helm-make-projectile
  "pM" #'helm-make
  "pG" #'projectile-regenerate-tags
  "p C-g" nil
  "qf" #'spacemacs/frame-killer
  "sJ" #'yxl-imenu-anywhere
  "tv" #'visual-fill-column-mode
  "ws" #'split-window-below-and-focus
  "wS" #'split-window-below
  "wv" #'split-window-right-and-focus
  "wV" #'split-window-right
  "wY" #'yxl-window-record-layout
  "wP" #'yxl-window-load-layout
  "w M-h" #'buf-move-left
  "w M-j" #'buf-move-down
  "w M-k" #'buf-move-up
  "w M-l" #'buf-move-right
  "wx" #'kill-buffer-and-window
  "xh" #'highlight-regexp
  "xH" #'yxl-ov-highlighter/body
  "xa{" 'spacemacs/align-repeat-left-curly-brace
  "xa}" 'spacemacs/align-repeat-right-curly-brace
  "xa[" 'spacemacs/align-repeat-left-square-brace
  "xa]" 'spacemacs/align-repeat-right-square-brace)

(spacemacs/set-leader-keys
  "." #'yxl-hydra-system/body)

(spacemacs/declare-prefix "o" "user-own")
(spacemacs/set-leader-keys
  ";" #'counsel-M-x
  "o-" #'yxl-dired-popup
  "o <SPC>" #'delete-other-windows
  "of" #'yxl-hydra-find-file/body
  "og" #'yxl-helm-hotspot
  "oy" #'copy-file-name-to-clipboard
  "oo" #'yxl-hydra-hotspot/body
  "op" #'yxl-hydra-find-dir/body
  "oP" #'yxl-project-open
  "ot" #'ivy-todo
  "ow" #'yxl-hydra-window/body)

(spacemacs/declare-prefix "ob" "buffer")
(spacemacs/set-leader-keys
  "obb" #'ibuffer
  "obc" #'clone-indirect-buffer-other-window
  "obd" #'delete-other-windows
  "obn" #'yxl-buffer-inherit
  "obr" #'revert-buffer)

(spacemacs/declare-prefix "oc" "cite")
(spacemacs/set-leader-keys
  "occ" #'helm-bibtex
  "ocg" #'gscholar-bibtex)

(spacemacs/declare-prefix "od" "dictionary")
(spacemacs/set-leader-keys
  "odd" #'helm-dictionary
  "odb" #'bing-dict-brief
  "ods" #'synonyms)

(spacemacs/declare-prefix "oF" "Frames")
(spacemacs/set-leader-keys
  "oFM" #'make-frame
  "oFN" #'set-frame-name)

(spacemacs/declare-prefix "oi" "insert")
(spacemacs/set-leader-keys
  "ois" #'yas-insert-snippet)

(spacemacs/declare-prefix "om" "modes")
(spacemacs/set-leader-keys
  "omh" #'html-mode
  "oml" #'latex-mode
  "omm" #'markdown-mode
  "omM" #'gfm-mode
  "omo" #'org-mode
  "omp" #'python-mode
  "omr" #'R-mode
  "ome" #'emacs-lisp-mode)

(spacemacs/declare-prefix "os" "search")
(spacemacs/set-leader-keys
  "osg" #'helm-google-suggest)

(spacemacs/declare-prefix "ox" "text")
(spacemacs/set-leader-keys
  "oxp" #'counsel-yank-pop)
