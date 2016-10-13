(defvar yxl-simple-todo-task1 "task")

(defvar yxl-simple-todo-task2 nil)

(defvar yxl-simple-todo-task3 nil)

(defun yxl/set-simple-todo-task1 (input)
  (interactive "sSet simple todo task1: ")
  (setq yxl-simple-todo-task1 input))

(defun yxl/set-simple-todo-task2 (input)
  (interactive "sSet simple todo task2: ")
  (setq yxl-simple-todo-task2 input))

(defun yxl/set-simple-todo-task3 (input)
  (interactive "sSet simple todo task3: ")
  (setq yxl-simple-todo-task3 input))

(provide 'simple-todo)
