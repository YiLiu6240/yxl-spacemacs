(setq yxl-packages '((alarm :location local)
                     (simple-todo :location local)
                     (scratch-pop)))

(defun yxl/init-alarm ()
  (use-package alarm
    :init
    (progn
      (spacemacs/set-leader-keys "ota" #'alarm-clock)
      (spacemacs/set-leader-keys "otc" #'alarm-countdown)
      (spacemacs/set-leader-keys "otC" #'alarm-clock-cancel))))

(defun yxl/init-simple-todo ()
  (use-package simple-todo
    :init
    (progn
      (spacemacs/set-leader-keys "ot1" #'yxl/set-simple-todo-task1)
      (spacemacs/set-leader-keys "ot2" #'yxl/set-simple-todo-task2)
      (spacemacs/set-leader-keys "ot3" #'yxl/set-simple-todo-task3))))

(defun yxl/init-scratch-pop ()
  (use-package scratch-pop
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys ".s" #'scratch-pop))))
