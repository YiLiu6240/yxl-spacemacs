(defun yxl/set-frame-code ()
  (interactive)
  (set-frame-name "Code")
  (message "Frame set to %s" "Code"))

(defun yxl/set-frame-REPL ()
  (interactive)
  (set-frame-name "REPL")
  (message "Frame set to %s" "REPL"))

(defun yxl/set-frame-meta ()
  (interactive)
  (set-frame-name "Meta")
  (message "Frame set to %s" "Meta"))

(defun yxl/set-frame-config ()
  (interactive)
  (set-frame-name "Config")
  (message "Frame set to %s" "Config"))

(defun select-frame-code ()
  (interactive)
  (select-frame-by-name "Code")
  (message "select Frame %s" "Code"))

(defun select-frame-REPL ()
  (interactive)
  (select-frame-by-name "REPL")
  (message "select Frame %s" "REPL"))

(defun select-frame-meta ()
  (interactive)
  (select-frame-by-name "Meta")
  (message "select Frame %s" "Meta"))

(defun select-frame-config ()
  (interactive)
  (select-frame-by-name "Config")
  (message "select Frame %s" "Config"))

(defun yxl/frame-setup-1 ()
  "will not work on linux"
  (interactive)
  (yxl/set-frame-config)
  (make-frame) (yxl/set-frame-meta)
  (make-frame) (yxl/set-frame-REPL)
  (make-frame) (yxl/set-frame-code)
  (select-frame-code))

(defun yxl/frame-setup-2 ()
  "test"
  (interactive)
  ;; set the first frame to be "config",
  ;; there is something wrong with the initial frame
  ;; so set initial frame as a normal frame, not a fullscreen one
  (yxl/set-frame-config)
  (make-frame) (yxl/set-frame-REPL)
  (make-frame) (yxl/set-frame-code)
  (make-frame) (yxl/set-frame-meta)

  (select-frame-meta) (sleep-for 0.1)
  (toggle-frame-fullscreen) (sleep-for 0.5)
  (select-frame-REPL) (sleep-for 0.1)
  (toggle-frame-fullscreen) (sleep-for 0.5)
  (select-frame-code) (sleep-for 0.1)
  (toggle-frame-fullscreen) (sleep-for 0.5))

(provide 'yxl-frame)
