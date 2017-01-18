(require 'popup)
(require 'ess-site)



;; http://akhilsbehl.github.io/blog/2016/05/30/inspecting-objects-at-point-with-ess/

(defun asb-read-into-string (buffer)
  (with-current-buffer buffer
    (buffer-string)))

(defun asb-ess-R-object-popup (r-func)
  "R-FUNC: The R function to use on the object.
Run R-FUN for object at point, and display results in a popup."
  (let ((objname (current-word))
        (tmpbuf (get-buffer-create "**ess-R-object-popup**")))
    (if objname
        (progn
          (ess-command (concat "class(" objname ")\n") tmpbuf)
          (let ((bs (asb-read-into-string tmpbuf)))
            (if (not(string-match "\(object .* not found\)\|unexpected" bs))
                (progn
                  (ess-command (concat r-func "(" objname ")\n") tmpbuf)
                  (let ((bs (asb-read-into-string tmpbuf)))
                    (popup-tip bs)))))))
    (kill-buffer tmpbuf)))

(defun asb-ess-R-object-popup-str ()
  (interactive)
  (asb-ess-R-object-popup "str"))

(defun asb-ess-R-object-popup-interactive (r-func)
  (interactive "sR function to execute: ")
  (asb-ess-R-object-popup r-func))


;; https://github.com/emacs-ess/ESS/pull/391

(defun ess-beginning-of-pipe-or-end-of-line ()
  "Find point position of end of line or beginning of pipe %>%"
  (if (search-forward "%>%" (line-end-position) t)
      (let ((pos (progn
                   (beginning-of-line)
                   (search-forward "%>%" (line-end-position))
                   (backward-char 3)
                   (point))))
        (goto-char pos))
    (end-of-line)))

(defun ess-eval-pipe-through-line (vis)
  "Like `ess-eval-paragraph' but only evaluates up to the pipe on this line.
If no pipe, evaluate paragraph through the end of current line.
Prefix arg VIS toggles visibility of ess-code as for `ess-eval-region'."
  (interactive "P")
  (save-excursion
    (let ((end (progn
                 (ess-beginning-of-pipe-or-end-of-line)
                 (point)))
          (beg (progn (backward-paragraph)
                      (ess-skip-blanks-forward 'multiline)
                      (point))))
      (ess-eval-region beg end vis))))



(provide 'ess-goodies)
