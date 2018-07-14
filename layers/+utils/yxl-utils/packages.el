(setq yxl-utils-packages '(find-file-in-project
                           visual-fill-column
                           (yxl-invoke-applications :location site)
                           emamux
                           focus
                           helpful
                           (hexrgb :location site)
                           (helm-system-packages)))

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

(defun yxl-utils/init-helm-system-packages ()
  (use-package helm-system-packages
    :defer t))
