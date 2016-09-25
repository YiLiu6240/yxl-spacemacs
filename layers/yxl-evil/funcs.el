(defun yxl105/evil-insert-newline-around ()
  (interactive)
  (evil-insert-newline-above)
  (forward-line)
  (evil-insert-newline-below)
  (forward-line -1))

(defun yxl105/evil-insert-space ()
  "identical to vim: i SPC <escape> l"
  (interactive)
  (insert " "))

(defun yxl105/evil-apend-space ()
  "identical to vim: a SPC <escape> hh"
  (interactive)
  (forward-char 1)
  (insert " ")
  (forward-char -2))
