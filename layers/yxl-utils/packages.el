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
                           emamux
                           focus
                           helpful
                           define-word
                           (hexrgb :location local)
                           (magit-org-todos)))

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
               yxl-project-popup)))

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
  (use-package visual-fill-column))

(defun yxl-utils/init-yxl-invoke-applications ()
  (use-package yxl-invoke-applications))

(defun yxl-utils/init-emamux ()
  (use-package emamux
    :defer t))

(defun yxl-utils/init-focus ()
  (use-package focus
    :config
    (progn
      (setq focus-mode-to-thing '((prog-mode . defun)
                                  (text-mode . line))))))

(defun yxl-utils/init-helpful ()
  (use-package helpful
    :defer t))

(defun yxl-utils/init-define-word ()
  (use-package define-word
    :defer t
    :init (spacemacs/set-leader-keys "xD" #'define-word)))

(defun yxl-utils/init-hexrgb ()
  (use-package hexrgb
    :commands (hexrgb-rgb-to-hex
               hexrgb-hex-to-rgb
               convert-rgb-to-hex
               convert-hex-to-rgb)
    :config
    (progn
      (defun convert-rgb-to-hex (red green blue)
        "Usage: (convert-rgb-to-hex 235 219 178) => \"#EBDBB2\""
        (apply #'hexrgb-rgb-to-hex
               (append
                (mapcar
                 (lambda (x)
                   (/ (float x) (float 255)))
                 (list red green blue))
                (list 2))))
      (defun convert-hex-to-rgb (hex)
        "Useage (convert-hex-to-rgb \"#ebdbb2\") => c(235 219 178)"
        (mapcar
         (lambda (x)
           (round (* (float x) (float 255))))
         (hexrgb-hex-to-rgb hex))))))

(defun yxl-utils/init-magit-org-todos ()
  (use-package magit-org-todos
    :after magit
    :init
    (progn
      (setq magit-org-todos-filename "TODO.org"))
    :config
    (progn
      (defun magit-insert-standup-commits (&optional collapse)
        "Insert section showing recent commits. From yesterday to today."
        (let* ((range "--since=yesterday.midnight"))
          (magit-insert-section (recent range collapse)
            (magit-insert-heading "Standup")
            (magit-insert-log range
                              (cons (format "-n%d" magit-log-section-commit-count)
                                    (--remove (string-prefix-p "-n" it)
                                              magit-log-section-arguments))))))
      (magit-add-section-hook
       'magit-status-sections-hook
       'magit-insert-standup-commits
       'magit-insert-staged-changes
       t)
      (magit-add-section-hook
       'magit-status-sections-hook
       'magit-org-todos-insert-org-todos
       'magit-insert-staged-changes
       t))))
