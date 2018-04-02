(setq yxl-base-packages '((yxl-utils :location site)
                          (yxl-project :location site)
                          (yxl-window :location site)
                          (yxl-open :location site)
                          (general-goodies :location site)))

(defun yxl-base/init-yxl-utils ()
  (use-package yxl-utils
    :config
    (setq yxl-buffer-boring-buffer-regexp-list '("\\` "
                                                 "\\`\\*helm"
                                                 "\\`\\*Echo Area"
                                                 "\\`\\*Minibuf"
                                                 "*spacemacs*"))
    (setq yxl-buffer-inherit-whitelist '(latex-mode
                                         markdown-mode
                                         org-mode
                                         R-mode
                                         ess-julia-mode
                                         python-mode
                                         emacs-lisp-mode
                                         scala-mode
                                         clojure-mode))
    (setq yxl-buffer-inherit-special-alist '((ess-mode . R-mode)
                                             (inferior-ess-mode . R-mode)))))

(defun yxl-base/init-yxl-project ()
  (use-package yxl-project
    :commands (yxl-project-helm
               yxl-project-shell-popup
               yxl-project-cite
               yxl-project-select
               yxl-project-popup)))

(defun yxl-base/init-yxl-window ()
  (use-package yxl-window))

(defun yxl-base/init-yxl-open ()
  (use-package yxl-open
    :config
    (progn
      (setq browse-url-chromium-program "chromium-browser")
      (setq yxl-open-file-external-commands-linux
            '(("default" . (lambda (x) (browse-url x)))
              ("gvim" . (lambda (x) (yxl-open--linux-command "gvim" x)))
              ("subl" . (lambda (x) (yxl-open--linux-command "subl" x)))
              ("atom" . (lambda (x) (yxl-open--linux-command "atom" x)))
              ("zathura" . (lambda (x) (yxl-open--linux-command "zathura" x)))
              ("zathura-light" .
               (lambda (x)
                 (call-process-shell-command
                  "zathura" nil 0 nil
                  (format "%s \"%s\""
                          "-c ~/dotfiles/zathura-light"
                          x))))
              ("tad" . (lambda (x) (yxl-open--linux-command "tad" x)))
              ("vmd" . (lambda (x) (yxl-open--linux-command "vmd" x)))
              ("chrome" . (lambda (x) (browse-url-chrome x)))
              ("qutebrowser" . (lambda (x) (yxl-open--linux-command "qutebrowser" x)))
              ("chromium" . (lambda (x) (browse-url-chromium x)))
              ("firefox" . (lambda (x) (browse-url-firefox x)))
              ("desktop" . (lambda (x) (yxl-open-in-desktop)))
              ("self exec" . (lambda (x)
                               (let ((dir-path (file-name-directory x)))
                                 (shell-command (format "cd %s && %s" dir-path x)))))
              ("dired" . (lambda (x) (dired-jump t x)))
              ("directory in terminal" . (lambda (x) (yxl-open-in-terminal)))))
      (setq yxl-open-file-external-commands-darwin
            '(("default" . (lambda (x) (browse-url x)))
              ("gvim" . (lambda (x) (yxl-open--darwin-command "gvim" x)))
              ("subl" . (lambda (x) (yxl-open--darwin-command "subl" x)))
              ("atom" . (lambda (x) (yxl-open--darwin-command "atom" x)))
              ("zathura" . (lambda (x) (yxl-open--darwin-command "zathura" x)))
              ("browser" . (lambda (x) (yxl-open--darwin-command "open -a \"Google Chrome\"" x)))
              ("desktop" . (lambda (x) (yxl-open-in-desktop)))
              ("dired" . (lambda (x) (dired-jump t x)))
              ("directory in terminal" . (lambda (x) (yxl-open-in-terminal))))))))

(defun yxl-base/init-general-goodies ()
  (use-package general-goodies))
