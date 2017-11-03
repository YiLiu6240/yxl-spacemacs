(defun yxl-utils/home ()
  "Force recreation of the spacemacs buffer.

Accepts C-u arg to delete all other windows."
  (interactive)
  (when (equal current-prefix-arg '(4))
    (delete-other-windows))
  (setq spacemacs-buffer--last-width nil)
  (spacemacs-buffer/goto-buffer t))

(defun yxl-utils/frame-zoom-in (&optional x)
  "zoom in frame by `x', but keep the same pixel size"
  (interactive)
  (let ((zoom-factor (or x 1)))
    (spacemacs//zoom-frm-do zoom-factor)
    (spacemacs//zoom-frm-powerline-reset)))

(defun yxl-utils/frame-zoom-out (&optional x)
  "zoom in frame by `x', but keep the same pixel size"
  (interactive)
  (let ((zoom-factor (- (or x 1))))
    (spacemacs//zoom-frm-do zoom-factor)
    (spacemacs//zoom-frm-powerline-reset)))

(defun yxl-utils/frame-zoom-state (&optional state)
  "zoom frame to `x', but keep the same pixel size"
  (let* ((state (or state 0))
         (fm (cdr (assoc 'fullscreen (frame-parameters))))
         (fwp (* (frame-char-width) (frame-width)))
         (fhp (* (frame-char-height) (frame-height)))
         (frame-scale (or (frame-parameter nil 'zoomed) 0)))
    (unless (eq frame-scale 0)
      (zoom-frm-unzoom))
    (dotimes (i state) (zoom-frm-in))
    (set-frame-size nil fwp fhp t)
    (when (equal fm 'maximized)
      (toggle-frame-maximized))
    (powerline-reset)))

(defun yxl-utils/goto-note-local ()
  ""
  (interactive)
  (find-file yxl-file-note-local))
