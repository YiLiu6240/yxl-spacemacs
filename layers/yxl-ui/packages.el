(setq yxl-ui-packages '(spaceline
                        (yxl-spaceline :toggle (bound-and-true-p spaceline))
                        (yxl-mode-line :location local
                                       :toggle (not (bound-and-true-p spaceline)))))

(defun yxl-ui/init-yxl-mode-line ()
  (use-package yxl-mode-line))

(defun yxl-ui/init-yxl-spaceline ()
  (use-package yxl-spaceline))
