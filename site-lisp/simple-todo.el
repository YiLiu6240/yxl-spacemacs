;; TODO: refactor
;; create a list for as a todo stack
;; popup a special buffer where each line is one todo task
;; visible the first two todo tasks on modeline

(defvar yxl-simple-todo-task1 "")

(defvar yxl-simple-todo-task2 "")

(defvar yxl-simple-todo-task3 "")

(defun yxl-set-simple-todo-task1 (input)
  (interactive "sSet simple todo task1: ")
  (setq yxl-simple-todo-task1 input))

(defun yxl-set-simple-todo-task2 (input)
  (interactive "sSet simple todo task2: ")
  (setq yxl-simple-todo-task2 input))

(defun yxl-set-simple-todo-task3 (input)
  (interactive "sSet simple todo task3: ")
  (setq yxl-simple-todo-task3 input))

(provide 'simple-todo)
