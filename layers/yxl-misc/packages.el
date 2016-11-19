(setq yxl-misc-packages '(ereader))

(defun yxl-misc/init-ereader ()
  (use-package ereader
    :defer t
    :commands (ereader-mode)
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.epub$" . ereader-mode)))))
