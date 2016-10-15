;; TODO:
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Marks-vs-Flags.html#Marks-vs-Flags
(defun yxl-dired/hydra-setup ()
  (defhydra hydra-dired-common (:color blue)
    ("q" nil "quit")
    ("." nil "quit"))
  (defhydra hydra-dired-mark
    (:hint nil :color blue :inherit (hydra-dired-common/heads))
    "
[_*_] mark exe"
    ("*" dired-mark-executables))
  (defhydra hydra-dired-main
    (:hint nil :color blue :inherit (hydra-dired-common/heads))
    "
[_S_] sort+
[_m_] mark [_M_] mark+\n"
    ("S" hydra-dired-quick-sort/body)
    ("m" dired-mark :color red)
    ("M" hydra-dired-mark/body))

  (define-key dired-mode-map "." 'hydra-dired-main/body))
