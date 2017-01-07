;; source:
;;   https://www.emacswiki.org/emacs/Calfw
;;   https://github.com/kiwanami/emacs-calfw

(defvar calfw-packages '(calfw)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun calfw/init-calfw()
  (use-package calfw
    :defer t
    :commands (cfw-open-calendar)
    :config
    (progn
      (evil-set-initial-state 'cfw:calendar-mode 'evilified)
      (require 'calfw-cal)
      (require 'calfw-ical)
      (require 'calfw-org)
      (calfw/setup-bindings)

      (defun cfw-open-calendar()
        (interactive)
        (cfw:open-calendar-buffer
         :contents-sources
         (list
          (cfw:org-create-source (face-attribute 'default :foreground)))))

      (calendar-set-date-style 'iso))))
