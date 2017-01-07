(defun calfw/setup-bindings ()
  (evilified-state-evilify-map cfw:calendar-mode-map
    :mode cfw:calendar-mode
    :bindings

    (kbd "<right>") #'cfw:navi-next-day-command
    "f"       #'cfw:navi-next-day-command
    (kbd "<left>")  #'cfw:navi-previous-day-command
    "b"       #'cfw:navi-previous-day-command
    (kbd "<down>")  #'cfw:navi-next-week-command
    "n"       #'cfw:navi-next-week-command
    (kbd "<up>")    #'cfw:navi-previous-week-command
    "p"       #'cfw:navi-previous-week-command

    ;; Vi style
    "l" #'cfw:navi-next-day-command
    "h" #'cfw:navi-previous-day-command
    "j" #'cfw:navi-next-week-command
    "k" #'cfw:navi-previous-week-command
    "^" #'cfw:navi-goto-week-begin-command
    "$" #'cfw:navi-goto-week-end-command

    "<"   #'cfw:navi-previous-month-command
    (kbd "M-v") #'cfw:navi-previous-month-command
    ">"   #'cfw:navi-next-month-command
    (kbd "C-v") #'cfw:navi-next-month-command
    (kbd "<prior>") #'cfw:navi-previous-month-command
    (kbd "<next>") #'cfw:navi-next-month-command
    (kbd "<home>") #'cfw:navi-goto-first-date-command
    (kbd "<end>") #'cfw:navi-goto-last-date-command

    "g" #'cfw:navi-goto-date-command
    "t" #'cfw:navi-goto-today-command
    "." #'cfw:navi-goto-today-command

    (kbd "TAB") #'cfw:navi-next-item-command

    "r" #'cfw:refresh-calendar-buffer
    "o" #'cfw:show-details-command
    ;; "o" cfw:org-open-agenda-day

    "D" #'cfw:change-view-day
    "W" #'cfw:change-view-week
    "T" #'cfw:change-view-two-weeks
    "M" #'cfw:change-view-month

    ;; [mouse-1] #'cfw:navi-on-click

    "q" #'bury-buffer

    "0" #'digit-argument
    "1" #'digit-argument
    "2" #'digit-argument
    "3" #'digit-argument
    "4" #'digit-argument
    "5" #'digit-argument
    "6" #'digit-argument
    "7" #'digit-argument
    "8" #'digit-argument
    "9" #'digit-argument)
  (evilified-state-evilify-map cfw:details-mode-map
    :mode cfw:details-mode
    :bindings
    "q"       #'cfw:details-kill-buffer-command
    "o"       #'cfw:details-kill-buffer-command
    "n"       #'cfw:details-navi-next-command
    "f"       #'cfw:details-navi-next-command
    (kbd "<right>") #'cfw:details-navi-next-command
    "p"       #'cfw:details-navi-prev-command
    "b"       #'cfw:details-navi-prev-command
    (kbd "<left>")  #'cfw:details-navi-prev-command
    (kbd "TAB")     #'cfw:details-navi-next-item-command))
