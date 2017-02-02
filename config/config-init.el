;; project stuff
(defvar-local master-dir nil)

;; (push '(width . 84) initial-frame-alist)
;; (push '(height . 60) initial-frame-alist)
;; (push '(width . 84) default-frame-alist)
;; (push '(height . 60) default-frame-alist)

(if (not (display-graphic-p))
    ;; TODO: tweak various bg settings in TUI
    (setq yxl-theme-set-bg nil))

(if (eq system-type 'windows-nt)
    (progn
      ;; use external ls
      ;; use `where' as equivalent of `which'
      (setq ls-lisp-use-insert-directory-program t)
      (setq insert-directory-program "c:/tools/msys64/usr/bin/ls")
      (setenv "PATH" (concat "C:\\tools\\msys64\\mingw64\\bin;" (getenv "PATH")))))
(if (eq system-type 'darwin)
    ;; osx setup of pdf-tools
    ;; http://emacs.stackexchange.com/questions/13314/install-pdf-tools-on-emacs-macosx
    (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo"))
