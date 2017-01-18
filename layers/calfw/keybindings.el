(with-eval-after-load 'calfw
 (evilified-state-evilify-map cfw:calendar-mode-map
   :mode cfw:calendar-mode
   :bindings

   "f" #'cfw:navi-next-day-command
   "b" #'cfw:navi-previous-day-command
   "n" #'cfw:navi-next-week-command
   "p" #'cfw:navi-previous-week-command

   "l" #'cfw:navi-next-day-command
   "h" #'cfw:navi-previous-day-command
   "j" #'cfw:navi-next-week-command
   "k" #'cfw:navi-previous-week-command

   "^" #'cfw:navi-goto-week-begin-command
   "$" #'cfw:navi-goto-week-end-command
   "H" #'cfw:navi-goto-week-begin-command
   "L" #'cfw:navi-goto-week-end-command

   "K"   #'cfw:navi-previous-month-command
   "J"   #'cfw:navi-next-month-command
   "<"   #'cfw:navi-previous-month-command
   ">"   #'cfw:navi-next-month-command

   "g" #'cfw:navi-goto-date-command
   "t" #'cfw:navi-goto-today-command

   (kbd "TAB") #'cfw:navi-next-item-command

   "r" #'cfw:refresh-calendar-buffer
   "o" #'cfw:show-details-command
   "O" #'cfw:org-open-agenda-day

   "D" #'cfw:change-view-day
   "W" #'cfw:change-view-week
   "T" #'cfw:change-view-two-weeks
   "M" #'cfw:change-view-month

   "." #'cfw-calendar-hydra/body
   "q" #'bury-buffer)

 (evilified-state-evilify-map cfw:details-mode-map
   :mode cfw:details-mode
   :bindings
   "q"       #'cfw:details-kill-buffer-command
   "o"       #'cfw:details-kill-buffer-command
   "l"       #'cfw:details-navi-next-command
   "h"       #'cfw:details-navi-prev-command
   (kbd "TAB")     #'cfw:details-navi-next-item-command))

(defhydra cfw-calendar-hydra (:color red :hint nil)
  "
 | Navigation ^^^^       | View            | Goto               |
 | [_b_/_h_]: prev day   | [_D_]: Day      | [_o_]: open        |
 | [_f_/_l_]: next day   | [_W_]: Week     | [_O_]: open agenda |
 | [_p_/_k_]: prev week  | [_T_]: Two-week | [_g_]: goto        |
 | [_n_/_j_]: next week  | [_M_]: Month    | [_t_]: today       |
 | [_H_]: week start ^^  | ^^              | [_TAB_]: item      |
 | [_L_]: week end   ^^  | ^^              | ^^                 |
 | [_<_/_K_]: prev month | ^^              | ^^                 |
 | [_>_/_J_]: next month | ^^              | ^^                 |
"

  ("f" cfw:navi-next-day-command)
  ("b" cfw:navi-previous-day-command)
  ("n" cfw:navi-next-week-command)
  ("p" cfw:navi-previous-week-command)

  ("l" cfw:navi-next-day-command)
  ("h" cfw:navi-previous-day-command)
  ("j" cfw:navi-next-week-command)
  ("k" cfw:navi-previous-week-command)

  ("^" cfw:navi-goto-week-begin-command)
  ("$" cfw:navi-goto-week-end-command)
  ("H" cfw:navi-goto-week-begin-command)
  ("L" cfw:navi-goto-week-end-command)

  ("K"   cfw:navi-previous-month-command)
  ("J"   cfw:navi-next-month-command)
  ("<"   cfw:navi-previous-month-command)
  (">"   cfw:navi-next-month-command)

  ("g" cfw:navi-goto-date-command)
  ("t" cfw:navi-goto-today-command)

  ("TAB" cfw:navi-next-item-command)

  ("o" cfw:show-details-command :color blue)
  ("O" cfw:org-open-agenda-day :color blue)

  ("D" cfw:change-view-day)
  ("W" cfw:change-view-week)
  ("T" cfw:change-view-two-weeks)
  ("M" cfw:change-view-month)

  ("q" nil "quit" :color blue)
  ("r" cfw:refresh-calendar-buffer "refresh"))
