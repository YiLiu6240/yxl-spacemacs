;; https://github.com/twillis/my-emacs-config/blob/master/local/alarm.el

;;alarm clock functions
(defvar alarm-clock-timer nil
  "Keep timer so that the user can cancel the alarm")

(defvar alarm-countdown-remaining nil)
(defvar alarm-update nil)

(defvar alarm-update-interval 5)

(defun alarm-clock-message (text)
  "The actual alarm action"
  (message-box "%s \n  %s" text (format-time-string "%H:%M" (current-time))))

(defun alarm-countdown-update (due)
  (setq alarm-countdown-remaining
        (format-time-string "%M:%S" (time-subtract due (current-time)))))

(defun alarm-countdown-remove ()
  (setq alarm-countdown-remaining nil))

(defun alarm-clock ()
  "Set an alarm.
The time format is the same accepted by `run-at-time'.  For
example \"11:30am\"."
  (interactive)
  (let ((time (read-string "Time: "))
        (text (read-string "Alarm message: ")))
    (setq alarm-clock-timer (run-at-time time nil 'alarm-clock-message text))))

(defun alarm-countdown ()
  (interactive)
  (let* ((time (read-string "Time (minutes): "))
        (text (read-string "Alarm message: "))
        (due (time-add (current-time)
                       (seconds-to-time (* (string-to-number time)
                                           60)))))
    (setq alarm-clock-timer (run-at-time due nil 'alarm-clock-message text))
    (setq alarm-clock-timer (run-at-time due nil 'alarm-countdown-remove))
    (setq alarm-countdown-remaining
          (format-time-string "%M:%S" (time-subtract due (current-time))))
    (setq alarm-update (run-at-time t alarm-update-interval
                                    'alarm-countdown-update due))))

(defun alarm-clock-cancel ()
  "Cancel the alarm clock"
  (interactive)
  (cancel-timer alarm-clock-timer)
  (message "alarm clock cancelled"))

(provide 'alarm)
