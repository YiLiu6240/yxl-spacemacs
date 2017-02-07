(setq yxl-utils-packages '((yxl-utils :location site)
                           (general-goodies :location site)
                           find-file-in-project
                           (simple-todo :location site)
                           (scratch-pop :location site)
                           key-chord))

(defun yxl-utils/init-yxl-utils ()
  (use-package yxl-utils
    :config
    (progn
      (setq yxl-hhs-org-files yxl-env-org-files)
      (setq yxl-hhs-file-local-list yxl-file-sites-local)
      (setq yxl-hhs-file-web-list yxl-file-sites-web)
      (setq yxl-hhs-file-reading-list-local yxl-file-reading-list-files)
      (setq yxl-hhs-file-reading-list-webpages yxl-file-reading-list-webpages)
      (setq yxl-org-task-files yxl-env-org-files))))

(defun yxl-utils/init-general-goodies ()
  (use-package general-goodies))

(defun yxl-utils/init-find-file-in-project ()
  (use-package find-file-in-project
    :defer t
    :config
    (progn
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
;;       (spacemacs/set-leader-keys "ot3" #'yxl-set-simple-todo-task3))))

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

(defun yxl-utils/init-key-chord ()
  (use-package key-chord
    :init
    (progn (key-chord-mode 1))
    :config
    (progn)))
      ;; (setq key-chord-two-keys-delay 0.015)
      ;; (setq key-chord-one-key-delay 0.020)
