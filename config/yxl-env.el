(defvar yxl-env-freq-projects-alist '()
  "Project paths.
Alist in the form of (PATH . key) as (\"~/Downloads\" . \"D\")
to be pass to `yxl-hydra-projects'")

(defvar yxl-env-freq-files-alist '()
  "Files of interest.
Alist of the same format to `yxl-env-freq-projects-alist'
to be passed to `yxl-hydra-files.'")

;; FOLLOW: update this when the org-directory bug
;;         has been fixed
(defvar yxl-env-org-directory "~/org/"
  "Org directory.")

(defvar yxl-env-org-todo nil
  "Master todo file.")

(defvar yxl-env-org-checkbox nil
  "Master checkbox file.")

(defvar yxl-env-org-log nil
  "Master log file.")

(defvar yxl-env-bib nil
  "Master bibliography file.")

(defvar yxl-env-note-local nil
  "Local note file.")

(defvar yxl-env-note-sync nil
  "Synchronised note file.")
