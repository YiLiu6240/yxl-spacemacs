(defun yxl-edit/setup-lispy-bindings-for-map (map)
  (define-key map
    (kbd "C-<tab>") #'lispy-indent-adjust-parens)
  (define-key map
    (kbd "C-S-<tab>") #'lispy-dedent-adjust-parens)
  (define-key map
    (kbd "<C-iso-lefttab>") #'lispy-dedent-adjust-parens))
