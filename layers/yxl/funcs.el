(defun nuke-all-buffers ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapcar (lambda (x) (kill-buffer x)) (buffer-list))
  (delete-other-windows))

;; frame functions
(defun set-frameGroup-Code ()
  (interactive)
  (set-frame-name "Code")
  (message "Frame set to %s" "Code"))
(defun set-frameGroup-REPL ()
  (interactive)
  (set-frame-name "REPL")
  (message "Frame set to %s" "REPL"))
(defun set-frameGroup-Meta ()
  (interactive)
  (set-frame-name "Meta")
  (message "Frame set to %s" "Meta"))
(defun set-frameGroup-Config ()
  (interactive)
  (set-frame-name "Config")
  (message "Frame set to %s" "Config"))
(defun select-frameGroup-Code ()
  (interactive)
  (select-frame-by-name "Code")
  (message "select Frame %s" "Code"))
(defun select-frameGroup-REPL ()
  (interactive)
  (select-frame-by-name "REPL")
  (message "select Frame %s" "REPL"))
(defun select-frameGroup-Meta ()
  (interactive)
  (select-frame-by-name "Meta")
  (message "select Frame %s" "Meta"))
(defun select-frameGroup-Config ()
  (interactive)
  (select-frame-by-name "Config")
  (message "select Frame %s" "Config"))

;; custom layouts
(defun yxl/custom-layout-1 ()
  "window layout 1 | 2/3"
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (windmove-right)
  (split-window-below)
  (windmove-left))
(defun yxl/custom-layout-2 ()
  "window layout 1/2 | 3/4"
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (windmove-right)
  (split-window-below)
  (windmove-left)
  (split-window-below))
(defun yxl/custom-layout-3 ()
  "window layout 1/2 | 3/4/5"
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (split-window-below)
  (windmove-right)
  (split-window-below)
  (split-window-below)
  (windmove-left))

;; quick layouts
(defun yxl/view-today-sidebar ()
  "show today task window, as a sidebar"
  (interactive)
  (split-window-right)
  (find-file yxl/org-file-master)
  (evil-window-set-width 30))
(defun yxl/view-todo-panel ()
  "open task windows: today, monthly, projects"
  (interactive)
  (yxl/custom-layout-1)
  (find-file yxl/org-file-master)
  (windmove-right)
  (cfw-open-calendar))

;; window functions
(defun yxl/split-window-below (&optional size)
  "split window with customized prefixes.
If C-u, create a window with 20 lines.
If C-u C-u, create a window with 15 lines.
Else create a window with lines parsed by prefix"
  (interactive "P")
  (cond
   ((equal size nil)
    (split-window-below))
   ((equal size '(4)) ; C-u
    (split-window-below -20))
   ((equal size '(16)) ; C-u C-u
    (split-window-below -15))
   (t (split-window-below size))))

(defun yxl/split-window-right (&optional size)
  "split window with customized prefixes.
If C-u, create a window with 20 lines.
If C-u C-u, create a window with 15 lines.
Else create a window with lines parsed by prefix"
  (interactive "P")
  (cond
   ((equal size nil)
    (split-window-right))
   ((equal size '(4)) ; C-u
    (split-window-right -20))
   ((equal size '(16)) ; C-u C-u
    (split-window-right -15))
   (t (split-window-right size))))

(defun yxl/split-window-below-and-focus (&optional size)
  "Split the window vertically and focus the new window."
  (interactive "P")
  (yxl/split-window-below size)
  (windmove-down))

(defun yxl/split-window-right-and-focus (&optional size)
  "Split the window vertically and focus the new window."
  (interactive "P")
  (yxl/split-window-right size)
  (windmove-right))

(defun split-window-below-small ()
  "split into a smaller window below, and move to focus"
  ;; TODO: create a variable for the "-20",
  ;; default to -20, if no numeric argument present
  (interactive)
  (if current-prefix-arg
      (list
       (yxl/split-window-below -20)
       (windmove-down)
       (spacemacs/new-empty-buffer))
    (list
     (yxl/split-window-below -20)
     (windmove-down))))

(defun split-window-right-small ()
  "split into a smaller window below, and move to focus"
  ;; TODO: create a variable for the "-20",
  ;; default to -20, if no numeric argument present
  (interactive)
  (if current-prefix-arg
      (list
        (yxl/split-window-right -35)
        (windmove-right)
        (spacemacs/new-empty-buffer))
    (list
     (yxl/split-window-right -35))))

(defun split-window-above-small ()
  "split into a smaller window above, and move to focus"
  (interactive)
  (if current-prefix-arg
      (list
       (yxl/split-window-below 20)
       (spacemacs/new-empty-buffer))
    (list
     (yxl/split-window-below 20))))

(defun split-window-left-small ()
  "split into a smaller window above, and move to focus"
  (interactive)
  (if current-prefix-arg
      (list
       (yxl/split-window-right 35)
       (windmove-right)
       (spacemacs/new-empty-buffer))
    (list
     (yxl/split-window-right 35)
     (windmove-right))))

;; find project functions
(defun yxl/find-dir-dotfiles ()
  (interactive)
  (find-file yxl/dotfiles))
(defun yxl/find-dir-Downloads ()
  (interactive)
  (find-file yxl/Downloads))
(defun yxl/find-dir-Dropbox ()
  (interactive)
  (find-file yxl/Dropbox))
(defun yxl/find-dir-org ()
  (interactive)
  (find-file yxl/org-directory))
(defun yxl/find-pwd-code ()
  (interactive)
  (find-file yxl/code-pwd))
(defun yxl/find-pwd-paper ()
  (interactive)
  (find-file yxl/paper-pwd))
(defun yxl/find-pwd-journal ()
  (interactive)
  (find-file yxl/journal-pwd))

;; find file functions
(defun yxl/find-file-bib ()
  (interactive)
  (find-file yxl/file-bib))

(defun yxl/find-file-org ()
  "visit org file(s). when no prefix, visit master org file;
when C-u, visit work org; when C-u 1, visit both in a dual pane. "
  (interactive)
  (if current-prefix-arg
      (cond
       ((equal current-prefix-arg 1)
        (list
         (delete-other-windows)
         (find-file yxl/org-file-master)
         (split-window-right-and-focus)
         (find-file yxl/org-file-work)))
       (t
        (list
         (find-file yxl/org-file-work))))
    (find-file yxl/org-file-master)))

(defun yxl/find-file-org-work ()
  (interactive)
  (find-file yxl/org-file-work))

;; TODO: change to org version as in capture template
(defun yxl/find-file-diary ()
  (interactive)
  (find-file diary-file))

(defun yxl/find-file-note ()
  (interactive)
  (find-file yxl-text-note-file))

(defun yxl/find-file-note-master ()
  (interactive)
  (find-file yxl/file-note-master))

(defun yxl/change-window-width (width)
  (interactive "nwindow width: ")
  (evil-resize-window width t))

;; --------
;; eyebrowse
;; --------
(defun yxl/workspace-general (tag)
  (interactive "set workspace tag: ")
  (eyebrowse-create-window-config)
  (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) tag)
  (helm-projectile-switch-project))

(defun yxl/workspace-dotfile ()
  (interactive)
  (let* ((tag "config"))
    (eyebrowse-create-window-config)
    (spacemacs/find-dotfile)
    (yxl/custom-layout-1)
    (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) tag)))

(defun yxl/workspace-code ()
  (interactive)
  (let* ((tag "code"))
    (eyebrowse-create-window-config)
    (yxl/find-pwd-code)
    (yxl/custom-layout-1)
    (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) tag)))

(defun yxl/workspace-paper ()
  (interactive)
  (let* ((tag "paper"))
    (eyebrowse-create-window-config)
    (yxl/find-pwd-paper)
    (yxl/custom-layout-1)
    (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) tag)))
