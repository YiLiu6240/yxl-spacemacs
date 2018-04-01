(defun yxl-jupyter/help ()
  "Normal invoke calls `ein:pytools-request-help'.
When invoking with a prefix arg, manually input func."
  (interactive)
  (if current-prefix-arg
      (progn
        (let ((func (read-string "Enter func: ")))
          (ein:pytools-request-help (ein:get-kernel-or-error) func)))
    (ein:pytools-request-help (ein:get-kernel-or-error) (ein:object-at-point-or-error))))

(defun spacemacs/ein:worksheet-merge-cell-next ()
  (interactive)
  (ein:worksheet-merge-cell
   (ein:worksheet--get-ws-or-error) (ein:worksheet-get-current-cell) t t))

(defun spacemacs//concat-leader (key)
  (if dotspacemacs-major-mode-leader-key
      (concat dotspacemacs-major-mode-leader-key key)
    (concat "," key)))

(defun ein:notebook-save-notebook-override (notebook
                                            retry
                                            &optional callback cbargs)
  (let ((content (ein:content-from-notebook notebook)))
    (ein:events-trigger (ein:$notebook-events notebook)
                        'notebook_saving.Notebook)
    (ein:content-save content
                      #'ein:notebook-save-notebook-success
                      (list notebook callback cbargs)
                      #'ein:notebook-save-notebook-error
                      (list notebook))))

(defun ein:edit-cell-exit-override ()
  "Close the EIN source edit buffer, saving contents back to the
original notebook cell, unless being called via
`ein:edit-cell-abort'."
  (interactive)
  (let ((edit-buffer (current-buffer))
        (ws ein:src--ws)
        (cell ein:src--cell))
    (ein:remove-overlay)
    (when ein:src--allow-write-back
      (ein:edit-cell-save))
    (kill-buffer-and-window)))

(defun ein:worksheet-insert-cell-below-and-edit ()
  (interactive)
  (call-interactively #'ein:worksheet-insert-cell-below)
  (call-interactively #'ein:edit-cell-contents))

(defun ein:edit-cell-save-and-execute-and-exit ()
  (interactive)
  (call-interactively #'ein:edit-cell-save-and-execute)
  (call-interactively #'ein:edit-cell-exit))

(defun ein:worksheet-collapse-output ()
  "By default will collapse output (reversing
`ein:worksheet-set-output-visibility-all'). When invoke with a
prefix, uncollapse them"
  (interactive)
  (let ((current-prefix-arg (not current-prefix-arg)))
    (call-interactively #'ein:worksheet-set-output-visibility-all)))
