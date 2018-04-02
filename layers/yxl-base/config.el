(defvar yxl-base-freq-projects-alist '()
  "Project paths.
Alist in the form of (PATH . key) as (\"~/Downloads\" . \"D\")
to be pass to `yxl-hydra-projects'")

(defvar yxl-base-freq-files-alist '()
  "Files of interest.
Alist of the same format to `yxl-base-freq-projects-alist'
to be passed to `yxl-hydra-files.'")

;; FOLLOW: update this when the org-directory bug
;;         has been fixed
(defvar yxl-base-org-directory "~/org/"
  "Org directory.")

(defvar yxl-base-org-todo nil
  "Master todo file.")

(defvar yxl-base-org-checkbox nil
  "Master checkbox file.")

(defvar yxl-base-org-log nil
  "Master log file.")

(defvar yxl-base-bib nil
  "Master bibliography file.")

(defvar yxl-base-note-local nil
  "Local note file.")

(defvar yxl-base-note-sync nil
  "Synchronised note file.")
