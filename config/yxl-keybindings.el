;; mac: use super+3 as "#"
(global-set-key (kbd "s-3") (lambda () (interactive) (insert "#")))
(global-set-key (kbd "C-S-p") #'counsel-M-x)
(global-set-key (kbd "C-S-o") #'company-yasnippet)
(global-set-key (kbd "C-S-y") #'yas-insert-snippet)
(global-set-key (kbd "C-h") #'delete-backward-char)
(global-set-key (kbd "M-q") #'spacemacs/toggle-truncate-lines)

(global-set-key (kbd "C-s") #'save-buffer)

(global-set-key (kbd "M-<SPC>") #'yas-or-company)
(define-key isearch-mode-map "\C-h" #'isearch-delete-char)

(defun yas-or-company ()
  (interactive)
  (if company-mode
      (call-interactively #'company-yasnippet)
    (yas-insert-snippet)))

(defun yxl-buffer-compilation ()
  (interactive)
  (switch-to-buffer "*compilation*"))

(defun yxl-buffer-messages ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*Messages*")))

(defun yxl-buffer-home-dir ()
  (interactive)
  (find-file "~"))

(defun yxl-buffer-org-quick ()
  (interactive)
  (find-file yxl-file-org-quick))

(defun yxl-buffer-org-todo ()
  (interactive)
  (find-file yxl-file-org-todo))

(defun yxl-buffer-org-log ()
  (interactive)
  (find-file yxl-file-org-log))

(defun yxl-buffer-note-local ()
  (interactive)
  (find-file yxl-file-note-local))

(defun yxl-buffer-note-sync ()
  (interactive)
  (find-file yxl-file-note-sync))

(defun visual-fill-column-toggle-center-text ()
  (interactive)
  (setq visual-fill-column-center-text
        (not visual-fill-column-center-text))
  (message (format "visual-fill-column-center-text: %s"
                   visual-fill-column-center-text)))

(defun spacemacs/find-dotfile-follow-symlink ()
  "Edit the `dotfile', in the current window."
  (interactive)
  (find-file-existing (file-truename (dotspacemacs/location))))

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
  "bob" #'ibuffer
  "boc" #'yxl-buffer-compilation
  "boC" #'clone-indirect-buffer-other-window
  "boD" #'delete-other-windows
  "boh" #'yxl-buffer-home-dir
  "bol" #'yxl-buffer-org-log
  "bom" #'yxl-buffer-messages
  "boi" #'yxl-buffer-note-local
  "boI" #'yxl-buffer-note-sync
  "bon" #'yxl-buffer-inherit
  "boo" #'yxl-buffer-org-quick
  "boO" #'yxl-buffer-org-todo
  ;; TODO: rm this with next spacemacs update
  "bm" #'yxl-buffer-switch-same-major-mode
  "br" #'revert-buffer
  "bx" #'kill-buffer-and-window
  "fed" #'spacemacs/find-dotfile-follow-symlink
  "ff" #'yxl-find-file-counsel
  "fh" #'helm-find-files-vertical
  "fY" #'yxl-show-and-copy-buffer-filename-in-projectile
  "fO" #'spacemacs/open-file-or-directory-in-external-app ; with C-u open in desktop
  "fo" #'yxl-open-file-external
  "fp" #'counsel-projectile-find-file   ; respects .projectile subprojects
  "fP" #'counsel-git                    ; treats the git repo as the whole porject
  "f C-p" #'find-file-in-project-truename ; does not respect gitignore, thus can find ignored files
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
  "tv" #'visual-line-mode
  "tV" #'visual-fill-column-mode
  "t C-v" #'visual-fill-column-toggle-center-text
  "tp" nil
  "tpp" #'parinfer-toggle-mode
  "tP" #'spacemacs/toggle-smartparens
  "w0" #'delete-other-windows
  "w1" #'delete-other-windows
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
