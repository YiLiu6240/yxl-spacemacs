(setq yxl-utils-packages '((yxl-utils :location site)
                           (yxl-project :location site)
                           (yxl-window :location site)
                           (yxl-open :location site)
                           (general-goodies :location site)
                           find-file-in-project
                           ;; (simple-todo :location site)
                           (scratch-pop :location site)
                           visual-fill-column
                           (yxl-invoke-applications :location site)
                           el2markdown
                           emamux
                           focus
                           helpful
                           define-word))

(defun yxl-utils/init-yxl-utils ()
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

(defun yxl-utils/init-yxl-project ()
  (use-package yxl-project
    :commands (yxl-project-helm
               yxl-project-shell-popup
               yxl-project-cite
               yxl-project-select
               yxl-project-popup)
    :config
    (progn
      (setq yxl-project-list `(,yxl-path-dotfiles
                               ,yxl-path-downloads
                               "~/.emacs.d"
                               ,yxl-path-local
                               ,yxl-path-sync
                               ,yxl-path-projects
                               ,yxl-path-org
                               ,yxl-path-code-pwd
                               ,yxl-path-paper-pwd
                               ,yxl-path-journal-pwd
                               ,yxl-path-book-reference
                               ,(concat yxl-path-local "yxl_datascience")))
      (setq yxl-project-todo-global yxl-file-org-todo))))

(defun yxl-utils/init-yxl-window ()
  (use-package yxl-window))

(defun yxl-utils/init-yxl-open ()
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

(defun yxl-utils/init-general-goodies ()
  (use-package general-goodies))

(defun yxl-utils/init-find-file-in-project ()
  (use-package find-file-in-project
    :defer t
    :commands (find-file-in-project find-file-in-project-truename)
    :config
    (progn
      (defun find-file-in-project-truename ()
        "Use the true directory (in case of a symlinked file/dir.)"
        (interactive)
        (let ((default-directory (file-truename default-directory)))
          (find-file-in-project)))
      ;; need this otherwise spacemacs.d will not be searched
      (setq ffip-prune-patterns '(;; VCS
                                  "*/.git/*"
                                  "*/.svn/*"
                                  "*/.cvs/*"
                                  "*/.bzr/*"
                                  "*/.hg/*"
                                  ;; project misc
                                  "*.log"
                                  "*/bin/*"
                                  ;; Mac
                                  "*/.DS_Store/*"
                                  ;; Ctags
                                  "*/tags"
                                  "*/TAGS"
                                  ;; Global/Cscope
                                  "*/GTAGS"
                                  "*/GPATH"
                                  "*/GRTAGS"
                                  "*/cscope.files"
                                  ;; html/javascript/css
                                  "*/.npm/*"
                                  "*/.tmp/*" ; TypeScript
                                  "*/.sass-cache/*" ; SCSS/SASS
                                  "*/.idea/*"
                                  "*min.js"
                                  "*min.css"
                                  "*/node_modules/*"
                                  "*/bower_components/*"
                                  ;; Images
                                  "*.png"
                                  "*.jpg"
                                  "*.jpeg"
                                  "*.gif"
                                  "*.bmp"
                                  "*.tiff"
                                  "*.ico"
                                  ;; documents
                                  "*.doc"
                                  "*.docx"
                                  "*.pdf"
                                  ;; C/C++
                                  "*.obj"
                                  "*.o"
                                  "*.a"
                                  "*.dylib"
                                  "*.lib"
                                  "*.dll"
                                  "*.exe"
                                  ;; Java
                                  "*/.metadata*"
                                  "*/.gradle/*"
                                  "*.class"
                                  "*.war"
                                  "*.jar"
                                  ;; Emacs/Vim
                                  "*flymake"
                                  "*/#*#"
                                  ".#*"
                                  "*.swp"
                                  "*~"
                                  "*.elc"
                                  "*/.cask/*"
                                  ;; Python
                                  "*.pyc")))))

;; (defun yxl-utils/init-simple-todo ()
;;   (use-package simple-todo
;;     :init
;;     (progn
;;       (spacemacs/set-leader-keys "ot1" #'yxl-set-simple-todo-task1)
;;       (spacemacs/set-leader-keys "ot2" #'yxl-set-simple-todo-task2)
;;       (spacemacs/set-leader-keys "ot3)))" #'yxl-set-simple-todo-task3))))

(defun yxl-utils/init-scratch-pop ()
  (use-package scratch-pop
    ;; need this for autoload
    :commands (scratch-pop scratch-pop-sticky)
    :defer t
    :config
    (progn
      (setq scratch-pop-default-mode 'markdown-mode)
      (defun yxl-scratch-pop-top ()
        (interactive)
        (let ((scratch-pop-position 'top))
          (scratch-pop))))))

(defun yxl-utils/init-visual-fill-column ()
  (use-package visual-fill-column
    :defer t))

(defun yxl-utils/init-yxl-invoke-applications ()
  (use-package yxl-invoke-applications
    :defer t
    :commands (yxl-invoke-applications)
    :config
    (progn
      (setq yxl-ia-list '(("calendar" . calendar)
                          ("org-agenda" . org-agenda)
                          ("org-capture" . org-capture)
                          ("w3m" . w3m)
                          ("cfw-calendar" . cfw-calendar)
                          ("rss: elfeed" . elfeed)
                          ("note: deft" . spacemacs/deft)
                          ("email: mu4e" . mu4e)
                          ("my-org-log" . my-org-log)
                          ("my-org-quick" .
                           (lambda () (find-file yxl-file-org-quick)))
                          ("my-org-todo" .
                           (lambda () (find-file yxl-file-org-todo)))
                          ("counsel-dash" . counsel-dash)
                          ("helm-github-stars" . helm-github-stars)
                          ("helm-chrome-bookmarks" . helm-chrome-bookmarks)
                          ("helm-bibtex" . helm-bibtex)
                          ("gscholar-bibtex" . gscholar-bibtex)
                          ("helm-google-suggest" . helm-google-suggest))))))

(defun yxl-utils/init-el2markdown ()
  (use-package el2markdown
    :defer t))

(defun yxl-utils/init-emamux ()
  (use-package emamux
    :defer t))

(defun yxl-utils/init-focus ()
  (use-package focus
    :defer t))

(defun yxl-utils/init-helpful ()
  (use-package helpful
    :defer t))

(defun yxl-utils/init-define-word ()
  (use-package define-word
    :defer t
    :init (spacemacs/set-leader-keys "xD" #'define-word)))
