(defvar yxl-text-outline-file nil
  "outline file for a tex directory, default is nil")

(with-eval-after-load 'text-mode
  (add-hook 'text-mode-hook 'visual-line-mode)
  (add-hook 'text-mode-hook 'hl-todo-mode)
  (add-hook 'text-mode-hook (lambda ()
                              (setq indent-tabs-mode nil
                                    tab-width 2)))
  (with-eval-after-load 'evil-surround
    (add-hook 'text-mode-hook 'yxl-text/evil-surround-pairs)))
