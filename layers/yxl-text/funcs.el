(defun yxl-text/find-TeX-master ()
  "in order to set TeX-master, create a `.dir-locals.el' which contains
in the project root directory. "
  (interactive)
  ;; TODO: should detect if TeX-master is a string
  (if (not (eq t TeX-master))
      (find-file TeX-master)
    (message "TeX-master not set")))

(defun yxl-text/find-outline-file ()
  (interactive)
  (if (not (eq nil yxl-text-outline-file))
      (find-file yxl-text-outline-file)
    (yxl-text/find-TeX-master)))

(defun yxl-text/evil-surround-pairs ()
  (push '(?m . ("\\\(" . "\\\)")) evil-surround-pairs-alist)
  (push '(?M . ("\\\( " . " \\\)")) evil-surround-pairs-alist)
  (push '(?n . ("\\[" . "\\]")) evil-surround-pairs-alist)
  (push '(?N . ("\\[ " . " \\]")) evil-surround-pairs-alist)
  (push '(?s . ("[" . "]")) evil-surround-pairs-alist)
  (push '(?q . ("\"" . "\"")) evil-surround-pairs-alist)
  (push '(?w . ("'" . "'")) evil-surround-pairs-alist))
