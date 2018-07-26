;; -*- lexical-binding: t -*-

(defun spacemacs/python-execute-file (arg)
  "Execute a python script in a shell."
  (interactive "P")
  ;; set compile command to buffer-file-name
  ;; universal argument put compile buffer in comint mode
  (let ((universal-argument t)
        (compile-command (format "%s %s"
                                 (spacemacs/pyenv-executable-find
                                  python-shell-interpreter)
                                 (file-name-nondirectory buffer-file-name))))
    (if arg
        (call-interactively 'compile)
      (compile compile-command t)
      (with-current-buffer (get-buffer "*compilation*")
        (inferior-python-mode)))))

(defun spacemacs/python-execute-file-focus (arg)
  "Execute a python script in a shell and switch to the shell buffer in
 `insert state'."
  (interactive "P")
  (spacemacs/python-execute-file arg)
  (switch-to-buffer-other-window "*compilation*")
  (end-of-buffer)
  (evil-insert-state))

(defun spacemacs//python-setup-shell (&rest args)
  (if (spacemacs/pyenv-executable-find "ipython")
      (progn (setq python-shell-interpreter "ipython")
             (if (version< (replace-regexp-in-string
                            "[\r\n|\n]$" ""
                            (shell-command-to-string "ipython --version")) "5")
                 (setq python-shell-interpreter-args "-i")
               (setq python-shell-interpreter-args "--simple-prompt -i")))
    (progn
      (setq python-shell-interpreter-args "-i")
      (setq python-shell-interpreter "python"))))

(defun spacemacs/pyenv-executable-find (command)
  "Find executable taking pyenv shims into account.
If the executable is a system executable and not in the same path
as the pyenv version then also return nil.
This works around https://github.com/pyenv/pyenv-which-ext
"
  (if (executable-find "pyenv")
      (progn
        (let ((pyenv-string (shell-command-to-string
                             (concat "pyenv which " command)))
              (pyenv-version-name (string-trim (shell-command-to-string
                                                "pyenv version-name"))))
          (and (not (string-match "not found" pyenv-string))
               (string-match pyenv-version-name (string-trim pyenv-string))
               (string-trim pyenv-string))))
    (executable-find command)))

(defun spacemacs//python-default ()
  "Defaut settings for python buffers"
  (setq mode-name "Python"
        tab-width python-tab-width
        fill-column python-fill-column)
  (when (version< emacs-version "24.5")
    ;; auto-indent on colon doesn't work well with if statement
    ;; should be fixed in 24.5 and above
    (setq electric-indent-chars (delq ?: electric-indent-chars)))
  (setq-local comment-inline-offset 2)
  (spacemacs/python-annotate-pdb)
  ;; make C-j work the same way as RET
  (local-set-key (kbd "C-j") 'newline-and-indent))

;; from http://pedrokroger.net/2010/07/configuring-emacs-as-a-python-ide-2/
(defun spacemacs/python-annotate-pdb ()
  "Highlight break point lines."
  (interactive)
  (highlight-lines-matching-regexp "import \\(pdb\\|ipdb\\|pudb\\|wdb\\)")
  (highlight-lines-matching-regexp "\\(pdb\\|ipdb\\|pudb\\|wdb\\).set_trace()")
  (highlight-lines-matching-regexp "trepan.api.debug()"))

(defun yxl-elpy//inferior-python-mode-hook-setup ()
  ;; disable smartscan-mode to make M-p and M-n select
  ;; previous/next statement in python shell
  (when (featurep 'smartscan)
    (smartscan-mode -1))
  (setq indent-tabs-mode t)
  ;; for some reason, q is bound to exit even in insert state -> undo this
  (define-key evil-insert-state-local-map "q" 'self-insert-command))

(defun yxl-elpy//elpy-mode-hook-setup ()
  ;; after 2 seconds or C-tab
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.2)
  (define-key elpy-mode-map (kbd "C-<tab>") 'company-complete))

(defun yxl-elpy/insert-codecell-above ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (insert "# <codecell>\n")))

(defun yxl-elpy/insert-markdowncell-above ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (insert "# <markdowncell>\n")))

(defun spacemacs//python-imenu-create-index-use-semantic-maybe ()
  "Use semantic if the layer is enabled."
  (setq imenu-create-index-function 'spacemacs/python-imenu-create-index))

;; fix for issue #2569 (https://github.com/syl20bnr/spacemacs/issues/2569) and
;; Emacs 24.5 and older. use `semantic-create-imenu-index' only when
;; `semantic-mode' is enabled, otherwise use `python-imenu-create-index'
(defun spacemacs/python-imenu-create-index ()
  (if (bound-and-true-p semantic-mode)
      (semantic-create-imenu-index)
    (python-imenu-create-index)))

(defun python-shell-send-string-print (string &optional process msg)
  "Wrap STRING with print() before sending it to
`python-shell-send-string'."
  (interactive
   (list (read-string "Python command: ") nil t))
  (let ((wrapped-string (concat "print(" string ")")))
    (python-shell-send-string wrapped-string process msg)))
