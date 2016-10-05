;; Eyebrowse

;; Eyebrowse uses window-state objects (as returned by `window-state-get') to
;; store window configurations, so here are some utility functions to help us
;; analyse window-states.
;; it might make more sense to move these functions to a more general place

(defun spacemacs/window-state-window-p (object)
  "Return t if OBJECT is a window, as represented in window-state objects.
Note: this function doesn't test for real window objects, but for
representations of a window in a window-state object as returned by
`window-state-get'."
  (and (listp object)
       (memq (car object) '(leaf vc hc))))

(defun spacemacs/window-state-get-buffer (window)
  "Get WINDOW's buffer.
WINDOW is the representation of a window in a window-state object.
The returned value is the representation of a buffer in a window-state
object."
  (cdr (assq 'buffer window)))

(defun spacemacs/window-state-get-buffer-name (window)
  "Get WINDOW's buffer's name.
WINDOW is the representation of a window in a window-state object."
  (car (spacemacs/window-state-get-buffer window)))

(defun spacemacs/window-state-walk-windows-1 (window fn)
  "Helper function for `spacemacs/window-state-walk-windows'."
  ;; WINDOW is a misleading name. WINDOW is a list that can represent a window,
  ;; or a concatenation of several windows. window-state objects are weird.
  (let ((child-windows
         (-filter #'spacemacs/window-state-window-p window))
        (bare-window
         ;; if WINDOW contains more than one window, take only the first window
         (--take-while (not (spacemacs/window-state-window-p it))
                       window)))
    (--each child-windows
      (spacemacs/window-state-walk-windows-1 it fn))
    (push (funcall fn bare-window) result)))

(defun spacemacs/window-state-walk-windows (state fn)
  "Execute FN once for each window in STATE and make a list of the results.
FN is a function to execute.
STATE is a window-state object."
  (let (result)
    (spacemacs/window-state-walk-windows-1 (cdr state) fn)
    result))

(defun spacemacs/window-state-all-windows (state)
  "Get all windows contained in STATE.
STATE is a window-state object.
The returned windows are not actual window objects. They are windows as
represented in window-state objects."
  (spacemacs/window-state-walk-windows state #'identity))

(defun spacemacs/window-state-get-buffer-names (state)
  "Get names of all buffers saved in STATE.
STATE is a window-state object as returned by `window-state-get'."
  (delq nil (spacemacs/window-state-walk-windows state #'spacemacs/window-state-get-buffer-name)))

(defun spacemacs/window-state-get-buffers (state)
  "Get all buffers saved in STATE.
STATE is a window-state object as returned by `window-state-get'."
  ;; delq nil - removes buffers stored in STATE that don't exist anymore
  (delq nil (mapcar #'get-buffer (spacemacs/window-state-get-buffer-names state))))

(defun spacemacs/find-workspace (buffer)
  "Find Eyebrowse workspace containing BUFFER.
 If several workspaces contain BUFFER, return the first one. Workspaces are
 ordered by slot number.
 If no workspace contains
 BUFFER, return nil."
  ;; the second element of a workspace is its window-state object
  (--find (memq buffer (spacemacs/window-state-get-buffers (cadr it)))
          (eyebrowse--get 'window-configs)))

(defun spacemacs/display-in-workspace (buffer alist)
  "Display BUFFER's workspace.
 Return BUFFER's window, if exists, otherwise nil.
 If BUFFER is already visible in current workspace, just return its window
 without switching workspaces."
  (or (get-buffer-window buffer)
      (-when-let (workspace (spacemacs/find-workspace buffer))
        (eyebrowse-switch-to-window-config (car workspace))
        (get-buffer-window buffer))))

(defun spacemacs/goto-buffer-workspace (buffer)
  "Switch to BUFFER's window in BUFFER's workspace.
 If BUFFER isn't displayed in any workspace, display it in the current
 workspace, preferably in the current window."
  (interactive "B")
  (pop-to-buffer buffer '((;; reuse buffer window from some workspace
                           spacemacs/display-in-workspace
                           ;; fallback to display in current window
                           display-buffer-same-window)
                          (inhibit-same-window . nil))))


;; Eyebrowse transient state

(defun spacemacs//workspaces-ts-toggle-hint ()
  "Toggle the full hint docstring for the workspaces transient-state."
  (interactive)
  (setq spacemacs--ts-full-hint-toggle
        (logxor spacemacs--ts-full-hint-toggle 1)))

(defun spacemacs/workspaces-ts-rename ()
  "Rename a workspace and get back to transient-state."
  (interactive)
  (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) nil)
  (spacemacs/workspaces-transient-state/body))

(defun spacemacs//workspace-format-name (workspace)
  "Return a propertized string given a WORKSPACE name."
  (let* ((current (eq (eyebrowse--get 'current-slot) (car workspace)))
         (name (nth 2 workspace))
         (number (car workspace))
         (caption (if (< 0 (length name))
                      (concat (int-to-string number) ":" name)
                    (int-to-string number))))
    (if current
        (propertize (concat "[" caption "]") 'face 'warning)
      caption)))

(defun spacemacs//workspaces-ts-hint ()
  "Return a one liner string containing all the workspaces names."
  (concat
   " "
   (mapconcat 'spacemacs//workspace-format-name
              (eyebrowse--get 'window-configs) " | ")
   (if (equal 1 spacemacs--ts-full-hint-toggle)
       spacemacs--workspaces-ts-full-hint
     (concat "  (["
             (propertize "?" 'face 'hydra-face-red)
             "] help)"))))
