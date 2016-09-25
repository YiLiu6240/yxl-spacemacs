(defvar yxl-text-outline-file yxl/org-file-work
  "outline file for a tex directory, default is nil")

(defvar yxl-text-note-file yxl/file-note-master
  "note file of a project, if not set, revert to master note file")

(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook 'hl-todo-mode)
(add-hook 'text-mode-hook (lambda ()
                            (setq indent-tabs-mode nil
                                  tab-width 2)))
(with-eval-after-load 'evil-surround
  (add-hook 'text-mode-hook 'yxl-text/evil-surround-pairs))
