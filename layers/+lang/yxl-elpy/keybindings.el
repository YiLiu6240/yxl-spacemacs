(defun yxl-elpy//setup-python-mode-leader-keys ()
  ;; python-mode key bindings
  (spacemacs/declare-prefix-for-mode 'python-mode "mg" "goto")
  (spacemacs/declare-prefix-for-mode 'python-mode "mh" "help")
  (spacemacs/declare-prefix-for-mode 'python-mode "ms" "send to REPL and step")
  (spacemacs/declare-prefix-for-mode 'python-mode "mV" "pyvenv")
  (spacemacs/declare-prefix-for-mode 'python-mode "mn" "notebook")
  (spacemacs/set-leader-keys-for-major-mode 'python-mode
    "'"  'spacemacs/python-start-or-switch-repl
    "cc" 'spacemacs/python-execute-file
    "cC" 'spacemacs/python-execute-file-focus
    "hh" 'elpy-doc
    "ga" 'elpy-goto-assignment
    "gA" 'elpy-goto-assignment-other-window
    "gg" 'elpy-goto-definition
    "go" 'elpy-occur-definitions
    "gG" 'elpy-goto-definition-other-window
    "gi" 'elpy-shell-switch-to-shell
    "gI" 'elpy-shell-switch-to-shell-in-current-window
    "se" 'elpy-shell-send-statement-and-step
    "sE" 'elpy-shell-send-statement-and-step-and-go
    "ss" 'elpy-shell-send-top-statement-and-step
    "sS" 'elpy-shell-send-top-statement-and-step-and-go
    "sf" 'elpy-shell-send-defun-and-step
    "sF" 'elpy-shell-send-defun-and-step-and-go
    "sc" 'elpy-shell-send-defclass-and-step
    "sC" 'elpy-shell-send-defclass-and-step-and-go
    "sg" 'elpy-shell-send-group-and-step
    "sG" 'elpy-shell-send-group-and-step-and-go
    "si" 'elpy-shell-switch-to-shell
    "sI" 'elpy-shell-switch-to-shell-in-current-window
    "sw" 'elpy-shell-send-codecell-and-step
    "sW" 'elpy-shell-send-codecell-and-step-and-go
    "sr" 'elpy-shell-send-region-or-buffer-and-step
    "sR" 'elpy-shell-send-region-or-buffer-and-step-and-go
    "sb" 'elpy-shell-send-buffer-and-step
    "sB" 'elpy-shell-send-buffer-and-step-and-go
    "Va" 'pyvenv-activate
    "Vd" 'pyvenv-deactivate
    "Vw" 'pyvenv-workon
    "nc" 'yxl-elpy/insert-codecell-above
    "nm" 'yxl-elpy/insert-markdowncell-above))

(defun yxl-elpy//setup-inferior-python-mode-leader-keys ()
  ;; inferior-python-mode key bindings
  (spacemacs/declare-prefix-for-mode 'inferior-python-mode "mg" "goto")
  (spacemacs/set-leader-keys-for-major-mode 'inferior-python-mode
    "gi" 'elpy-shell-switch-to-buffer
    "gI" 'elpy-shell-switch-to-buffer-in-current-window))

(defun yxl-elpy//setup-spacemacs-toggles ()
  ;; toggles
  (spacemacs/declare-prefix-for-mode 'python-mode "t" "toggles")
  (spacemacs|add-toggle yxl-elpy/shell-display-buffer-after-send
    :documentation "Toggles whether to show the python shell after sending something to it"
    :status elpy-shell-display-buffer-after-send
    :on (setq elpy-shell-display-buffer-after-send t)
    :off (setq elpy-shell-display-buffer-after-send nil)
    :evil-leader-for-mode (python-mode . "td"))
  (spacemacs|add-toggle yxl-elpy/shell-echo-input
    :documentation "Toggles whether to echo input sent to the Python shell in the shell buffer"
    :status elpy-shell-echo-input
    :on (setq elpy-shell-echo-input t)
    :off (setq elpy-shell-echo-input nil)
    :evil-leader-for-mode (python-mode . "ti"))
  (spacemacs|add-toggle yxl-elpy/shell-echo-output
    :documentation "Toggles whether to echo the Python shell output in the echo area"
    :status elpy-shell-echo-output
    :on (setq elpy-shell-echo-output 'when-shell-not-visible)
    :off (setq elpy-shell-echo-output nil)
    :evil-leader-for-mode (python-mode . "to")))
