;; Various functions taken from jkitchin's scimax (https://github.com/jkitchin/scimax)

(require 'ob-ipython)

(defun org-in-ipython-block-p (&optional inside)
  "Whether point is in a code source block.
When INSIDE is non-nil, don't consider we are within a src block
when point is at #+BEGIN_SRC or #+END_SRC."
  (let ((case-fold-search t))
    (or (and (equal (get-char-property (point) 'lang) "ipython"))
        (and (not inside)
             (save-excursion
               (beginning-of-line)
               (looking-at-p ".*#\\+\\(begin\\|end\\)_src ipython"))))))

(defun ob-ipython-signature-function (buffer pos)
  "Show a signature of the function at point in the minibuffer."
  (interactive (list (current-buffer) (point)))
  (save-restriction
    ;; Note you may be in a special edit buffer in which case it is not
    ;; necessary to narrow.
    (when (org-in-ipython-block-p) (org-narrow-to-block))
    (-if-let (result (->> (ob-ipython--inspect buffer
                                               (- pos (point-min)))
                          (assoc 'text/plain)
                          cdr))
        (progn
          (when (stringp result)
            (setq result (ansi-color-apply result)))
          (cond
           ((s-starts-with? "Signature:" result)
            (message (car (split-string result "\n"))))
           ((s-starts-with? "Docstring:" result)
            (message (s-join "\n" (-slice (split-string result "\n") 0 2))))
           (t
            (message (car (split-string result "\n"))))))
      (message "Nothing found"))))

(provide 'ob-ipython-goodies)
