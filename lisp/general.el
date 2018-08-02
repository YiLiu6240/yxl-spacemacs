(defun yas-or-company ()
  (interactive)
  (if company-mode
      (call-interactively #'company-yasnippet)
    (yas-insert-snippet)))

(defun yxl-buffer-compilation ()
  (interactive)
  (switch-to-buffer "*compilation*"))

(defun yxl-buffer-sticky-i3-window (file)
  "Visit a indirect buffer of FILE as a sticky i3 window.
Useful when we need two instances (one sticky note, and
one normal buffer."
  (interactive)
  (let* ((filename (file-name-nondirectory file))
         (sticky-name (concat "*sitcky*" filename))
         (buf (find-file-noselect file))
         (sticky-buf (get-buffer sticky-name)))
    (if sticky-buf
        (switch-to-buffer sticky-buf)
      (progn
        (switch-to-buffer (make-indirect-buffer buf sticky-name t))
        (text-scale-set 0)
        (spacemacs/toggle-mode-line-off)))))

(defun spacemacs/find-dotfile-follow-symlink ()
  "Edit the `dotfile', in the current window."
  (interactive)
  (find-file-existing (file-truename (dotspacemacs/location))))

(defun comint-toggle-scroll-to-bottom ()
    "Toggle the value of `comint-scroll-to-bottom-on-output'."
  (interactive)
  (setq-local comint-scroll-to-bottom-on-output
              (not comint-scroll-to-bottom-on-output))
  (message "comint-scroll-to-bottom-on-output: %s"
           comint-scroll-to-bottom-on-output))

(defun dired-jump-split ()
  "Split window below, focus, then invoke dired-jump."
  (interactive)
  (split-window-below)
  (windmove-down)
  (dired-jump))

(defun emacs-anywhere ()
  "Source:
https://github.com/zachcurry/emacs-anywhere/blob/master/emacs_anywhere.el"
  (interactive)
  ;; Currently only linux
  (let ((ea-on-delete (lambda (frame)
                        (call-process-region
                         (point-min) (point-max)
                         "xclip" nil 0 nil "-i" "-selection" "clipboard")))
        (ea-hook (lambda ()
                   (add-hook 'delete-frame-functions ea-on-delete))))
    (funcall ea-hook)
    (switch-to-buffer "*Emacs Anywhere*")
    (select-frame-set-input-focus (selected-frame))))

(defun my-org-log ()
  (interactive)
  (find-file yxl-base-org-log)
  (visual-fill-column-mode t)
  (setq visual-fill-column-center-text t))

(defun yxl-org-refile-to-scratch ()
  (interactive)
  (let ((org-refile-targets '((yxl-base-org-todo-life :maxlevel . 1)
                              (nil :maxlevel . 1))))
    (org-refile)))

(defun yxl-spacemacs-dashboard ()
  "The dashboard."
  (interactive)
  ;; Actions defer based on frame ratio
  (let* ((ratio-baseline 2.0)
         (frame-ratio (/ (float (frame-width)) (float (frame-height)))))
    (delete-other-windows)
    (if (< frame-ratio ratio-baseline)
        ;; vertical setup
        (progn
          (cfw/open-calendar)
          (split-window-below-and-focus)
          (org-todo-list)
          (with-current-buffer cfw:calendar-buffer-name
            (call-interactively 'cfw:refresh-calendar-buffer)))
      ;; horizontal setup
      (progn
        (cfw/open-calendar)
        (split-window-right-and-focus)
        (org-todo-list)
        (with-current-buffer cfw:calendar-buffer-name
          (call-interactively 'cfw:refresh-calendar-buffer))))))

(defun fuck-stargazer ()
  (interactive)
  (evil-ex-substitute (point-min) (point-max)
                      '("^\\( *\\\\\\\\\\\[-1.8ex\\\]\\)\\(.*\\)")
                      "\\1\n\\2" "/g")
  ;; gg=G
  (evil-indent (point-min) (point-max))
  (align (point-min) (point-max)))

(define-minor-mode nl2-mode
  "Highlight two successive newlines."
  :global t
  :lighter " nl2"
  (let ((pattern "\\(^\\s-*$\\)\n"))
    (if nl2-mode
        (highlight-regexp pattern 'hi-yellow)
      (unhighlight-regexp pattern))))

(provide 'general)
