(setq yxl-ui-packages '(spaceline))

(defun yxl-ui/post-init-spaceline ()
  (use-package spaceline-config
    :config
    (progn

      (setq dotspacemacs-mode-line-unicode-symbols nil
            spaceline-window-numbers-unicode nil
            spaceline-workspace-numbers-unicode nil)
      ;; (setq powerline-default-separator 'arrow)
      (setq powerline-default-separator nil)

      (spaceline-define-segment workspace-number
        "The current workspace name or number. Requires `eyebrowse-mode' to be
enabled."
        (when (bound-and-true-p eyebrowse-mode)
          (let* ((num (eyebrowse--get 'current-slot))
                 (tag (when num (nth 2 (assoc num (eyebrowse--get 'window-configs)))))
                 (str (if (and tag (< 0 (length tag)))
                          (concat "s" (int-to-string num) ":" tag)
                        (when num (concat "s" (int-to-string num))))))
            (or (when spaceline-workspace-numbers-unicode
                  (spaceline--unicode-number str))
                (propertize str 'face 'bold)))))

      (spaceline-define-segment window-number
        "The current window number. Requires `window-numbering-mode' to be enabled."
        (when (bound-and-true-p window-numbering-mode)
          (let* ((num (window-numbering-get-number))
                 (str (when num (concat (int-to-string num)))))
            (if spaceline-window-numbers-unicode
                (spaceline--unicode-number str)
              (propertize str 'face 'bold)))))

      (spaceline-define-segment which-function
        (when (and active
                   (bound-and-true-p which-function-mode)
                   (bound-and-true-p which-func-mode))
          (let* ((current (format-mode-line which-func-current)))
            (when (string-match "{\\(.*\\)}" current)
              (setq current (match-string 1 current)))
            (propertize current
                        'local-map which-func-keymap
                        'face 'which-func
                        'mouse-face 'mode-line-highlight
                        'help-echo (concat "mouse-1: go to beginning\n"
                                           "mouse-2: toggle rest visibility\n"
                                           "mouse-3: go to end")))))

      (spaceline-define-segment version-control
        "Version control information."
        (when vc-mode
          (powerline-raw
           (s-trim (concat vc-mode
                           (when (buffer-file-name)
                             (pcase (vc-state (buffer-file-name))
                               (`up-to-date " ")
                               (`edited " M")
                               (`added " A")
                               (`unregistered " ?")
                               (`removed " D")
                               (`needs-merge " C")
                               (`needs-update " U")
                               (`ignored " I")
                               (_ " -"))))))))

      (spaceline-define-segment yxl-simple-todo-task1-segment
        (let* ((str yxl-simple-todo-task1))
          (propertize str 'face 'font-lock-type-face)))

      (spaceline-define-segment yxl-simple-todo-task2-segment
        (when (not (eq yxl-simple-todo-task2 nil))
          (let* ((str yxl-simple-todo-task2))
            (propertize str 'face 'font-lock-string-face))))

      (spaceline-define-segment yxl-simple-todo-task3-segment
        (when (not (eq yxl-simple-todo-task3 nil))
          (let* ((str yxl-simple-todo-task3))
            (propertize str 'face 'font-lock-doc-face))))

      (spaceline-define-segment yxl-alarm-countdown-segment
        (when (not (eq alarm-countdown-remaining nil))
          (let* ((str alarm-countdown-remaining))
            (propertize str 'face 'font-lock-meta-face))))

      (spaceline-define-segment yxl-window-dedication-segment
        (let* ((dedicated (window-dedicated-p (selected-window)))
               (str (if dedicated "!" "")))
          (propertize str 'face 'font-lock-type-face)))

      ;; TODO: temporary measures
      (defun spaceline--pdfview-page-number ()
        (format "(%d/%d)"
                (pdf-view-current-page)
                (eval (pdf-view-current-page))
                (pdf-cache-number-of-pages)))

      (spaceline-compile
       'yxl
       ;; Left side of the mode line (all the important stuff)
       '(((window-number) :face highlight-face)
         (buffer-modified
          buffer-size
          buffer-id
          yxl-window-dedication-segment :tight)
         (workspace-number)
         anzu
         ((yxl-simple-todo-task1-segment :when active)
          (yxl-simple-todo-task2-segment :when active)
          (yxl-simple-todo-task3-segment :when active)
          (yxl-alarm-countdown-segment :when active))
         (which-function :when active)
         (process :when active)
         (flycheck-error flycheck-warning flycheck-info))
       ;; Right segment (the unimportant stuff)
       '(major-mode
         (minor-modes  :when active)
         (python-pyvenv :fallback python-pyenv)
         (battery :when active)
         input-method
         (version-control :when active)
         selection-info
         ((buffer-encoding-abbrev point-position line-column)
          :separator "|")
         ,@additional-segments
         buffer-position))

      (setq-default mode-line-format '("%e" (:eval (spaceline-ml-yxl))))

      (spaceline-toggle-minor-modes-off)
      ;; (spaceline-toggle-major-mode-off)
      ;; (spaceline-toggle-version-control-off)
      (spaceline-toggle-which-function-off)
      (spaceline-toggle-persp-name-off)
      (spaceline-toggle-version-control-off)
      (spaceline-toggle-buffer-size-off)
      )))
