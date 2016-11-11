(defun yxl-frame-set-code ()
  (interactive)
  (set-frame-name "Code")
  (message "Frame set to %s" "Code"))

(defun yxl-frame-set-repl ()
  (interactive)
  (set-frame-name "REPL")
  (message "Frame set to %s" "REPL"))

(defun yxl-frame-set-meta ()
  (interactive)
  (set-frame-name "Meta")
  (message "Frame set to %s" "Meta"))

(defun yxl-frame-set-config ()
  (interactive)
  (set-frame-name "Config")
  (message "Frame set to %s" "Config"))

(defun yxl-frame-select-code ()
  (interactive)
  (select-frame-by-name "Code")
  (message "select Frame %s" "Code"))

(defun yxl-frame-select-repl ()
  (interactive)
  (select-frame-by-name "REPL")
  (message "select Frame %s" "REPL"))

(defun yxl-frame-select-meta ()
  (interactive)
  (select-frame-by-name "Meta")
  (message "select Frame %s" "Meta"))

(defun yxl-frame-select-config ()
  (interactive)
  (select-frame-by-name "Config")
  (message "select Frame %s" "Config"))

(defun yxl-frame-setup-gen ()
  "will not work on linux"
  (interactive)
  (yxl-frame-set-config)
  (make-frame) (yxl-frame-set-meta)
  (make-frame) (yxl-frame-set-repl)
  (make-frame) (yxl-frame-set-code)
  (yxl-frame-select-code))

(defun yxl-frame-setup-mac ()
  "test"
  (interactive)
  ;; set the first frame to be "config",
  ;; there is something wrong with the initial frame
  ;; so set initial frame as a normal frame, not a fullscreen one
  (yxl-frame-set-config)
  (make-frame) (yxl-frame-set-repl)
  (make-frame) (yxl-frame-set-code)
  (make-frame) (yxl-frame-set-meta)

  (yxl-frame-select-meta) (sleep-for 0.1)
  (toggle-frame-fullscreen) (sleep-for 0.5)
  (yxl-frame-select-repl) (sleep-for 0.1)
  (toggle-frame-fullscreen) (sleep-for 0.5)
  (yxl-frame-select-code) (sleep-for 0.1)
  (toggle-frame-fullscreen) (sleep-for 0.5))

(provide 'yxl-frame)
