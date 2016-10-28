(with-eval-after-load 'calfw
  (setq cfw:calendar-mode-map
        (cfw:define-keymap
         '(("<right>" . cfw:navi-next-day-command)
           ("f"       . cfw:navi-next-day-command)
           ("<left>"  . cfw:navi-previous-day-command)
           ("b"       . cfw:navi-previous-day-command)
           ("<down>"  . cfw:navi-next-week-command)
           ("n"       . cfw:navi-next-week-command)
           ("<up>"    . cfw:navi-previous-week-command)
           ("p"       . cfw:navi-previous-week-command)

           ;; Vi style
           ("l" . cfw:navi-next-day-command)
           ("h" . cfw:navi-previous-day-command)
           ("j" . cfw:navi-next-week-command)
           ("k" . cfw:navi-previous-week-command)
           ("^" . cfw:navi-goto-week-begin-command)
           ("$" . cfw:navi-goto-week-end-command)

           ("<"   . cfw:navi-previous-month-command)
           ("M-v" . cfw:navi-previous-month-command)
           (">"   . cfw:navi-next-month-command)
           ("C-v" . cfw:navi-next-month-command)
           ("<prior>" . cfw:navi-previous-month-command)
           ("<next>"  . cfw:navi-next-month-command)
           ("<home>"  . cfw:navi-goto-first-date-command)
           ("<end>"   . cfw:navi-goto-last-date-command)

           ("g" . cfw:navi-goto-date-command)
           ("t" . cfw:navi-goto-today-command)
           ("." . cfw:navi-goto-today-command)

           ("TAB" . cfw:navi-next-item-command)

           ("r"   . cfw:refresh-calendar-buffer)
           ;; ("SPC" . cfw:show-details-command)
           ("o" . cfw:show-details-command)
           ("SPC" . spacemacs-cmds)

           ("D" . cfw:change-view-day)
           ("W" . cfw:change-view-week)
           ("T" . cfw:change-view-two-weeks)
           ("M" . cfw:change-view-month)

           ([mouse-1] . cfw:navi-on-click)

           ("q" . bury-buffer)

           ("0" . digit-argument)
           ("1" . digit-argument)
           ("2" . digit-argument)
           ("3" . digit-argument)
           ("4" . digit-argument)
           ("5" . digit-argument)
           ("6" . digit-argument)
           ("7" . digit-argument)
           ("8" . digit-argument)
           ("9" . digit-argument))))

  (setq cfw:details-mode-map
        (cfw:define-keymap
         '(("q"       . cfw:details-kill-buffer-command)
           ("o"     . cfw:details-kill-buffer-command)
           ("n"       . cfw:details-navi-next-command)
           ("f"       . cfw:details-navi-next-command)
           ("<right>" . cfw:details-navi-next-command)
           ("p"       . cfw:details-navi-prev-command)
           ("b"       . cfw:details-navi-prev-command)
           ("<left>"  . cfw:details-navi-prev-command)
           ("TAB"     . cfw:details-navi-next-item-command))))

  (setq cfw:org-schedule-map
        (cfw:define-keymap
         '(("q"   . bury-buffer)
           ("o" . cfw:org-open-agenda-day))))

  (custom-set-faces
   '(cfw:face-title ((t (:foreground "#f0dfaf" :weight bold :height 2.0 :inherit variable-pitch))))
   '(cfw:face-header ((t (:foreground "#d0bf8f" :weight bold))))
   '(cfw:face-sunday ((t :foreground "#cc9393" :background "#002b36" :weight bold)))
   '(cfw:face-saturday ((t :foreground "#8cd0d3" :background "#002b36" :weight bold)))
   '(cfw:face-holiday ((t :background "#002b36" :foreground "#cb4b16" :weight bold)))
   '(cfw:face-grid ((t :foreground "DarkGrey")))
   '(cfw:face-default-content ((t :foreground "#bfebbf")))
   '(cfw:face-periods ((t :foreground "#2aa198")))
   '(cfw:face-day-title ((t :background "#002b36")))
   '(cfw:face-default-day ((t :weight bold :inherit cfw:face-day-title)))
   '(cfw:face-annotation ((t :foreground "RosyBrown" :inherit cfw:face-day-title)))
   '(cfw:face-disable ((t :foreground "DarkGray" :inherit cfw:face-day-title)))
   '(cfw:face-today-title ((t :foreground "#002b36" :background "#859900" :weight bold)))
   '(cfw:face-today ((t :background: "#002b36" :weight bold)))
   '(cfw:face-select ((t :background "#2f2f2f")))
   ;; '(cfw:face-toolbar ((t :foreground "Steelblue4" :background "Steelblue4")))
   '(cfw:face-toolbar ((t :foreground "#073642" :background "#073642")))
   '(cfw:face-toolbar-button-off ((t :foreground "#839496" :weight bold)))
   '(cfw:face-toolbar-button-on ((t :foreground "#fdf6e3" :weight bold)))))
