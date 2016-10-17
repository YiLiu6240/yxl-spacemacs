(setq yxl-packages '((yxl :location local)
                     (goodies :location local)
                     (alarm :location local)
                     (simple-todo :location local)
                     ;; (scratch-pop :location (recipe :fetcher github
                     ;;                                :repo "YiLiu6240/scratch-pop"
                     ;;                                :branch "dev"))
                     (scratch-pop :location local)
                     bm))

(defun yxl/init-yxl ()
  (use-package yxl))

(defun yxl/init-goodies ()
  (use-package goodies))

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
    ;; need this for autoload
    :commands (scratch-pop scratch-pop-sticky)
    :defer t
    :config
    (progn
      (setq scratch-pop-default-mode 'gfm-mode)
      (defun yxl/scratch-pop-top ()
        (interactive)
        (let ((scratch-pop-position 'top))
          (scratch-pop))))))

(defun yxl/init-bm ()
  (use-package bm
    :defer t
    :config
    (progn
      (setq bm-repository-file
            (expand-file-name "~/Dropbox/inbox/.bm-repository"))
      (setq bm-cycle-all-buffers t))))
