(setq yxl-ui-packages '(airline-themes
                        (yxl-airline :location site)))

(defun yxl-ui/init-airline-themes ()
  (use-package airline-themes
    :ensure t
    :config
    (progn
      (setq airline-cursor-colors t)
      (setq airline-display-directory nil))))

(defun yxl-ui/init-yxl-airline ()
  (use-package yxl-airline))
