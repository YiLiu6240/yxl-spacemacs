(setq calfw-packages '(calfw
                       calfw-org))

(defun calfw/init-calfw ()
  (use-package calfw
    :commands (cfw/open-calendar
               cfw:open-calendar-buffer
               cfw/most-recent-date)
    :config
    (progn
      (evil-set-initial-state 'cfw:calendar-mode 'evilified)
      (calendar-set-date-style 'iso))))

(defun calfw/init-calfw-org ()
  (use-package calfw-org
    :after (calfw)
    :config
    (progn
      (setq cfw:org-face-agenda-item-foreground-color
            (face-foreground 'font-lock-type-face)))))
