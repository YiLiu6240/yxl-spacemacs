(setq yxl-workspace-packages '(
                               ;; eyebrowse
                               (yxl-ace-window :location site)
                               (yxl-session :location site)
                               buffer-move))

(defun yxl-workspace/init-yxl-ace-window ()
  (use-package yxl-ace-window
    :after (ace-window)
    :config
    (progn
      (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l
                         ?q ?w ?e ?r ?t ?y ?u ?i ?o ?p
                         ?z ?x ?c ?v ?b ?n ?m))
      (with-eval-after-load 'counsel
        (ivy-add-actions 'counsel-find-file
                         '(("O" yxl-ace-window-open "dispatch to an ace window")
                           ("s" yxl-ace-window-open-horz "split horz, ace window")
                           ("v" yxl-ace-window-open-vert "split horz, ace window")))))))

(defun yxl-workspace/init-buffer-move ()
  (use-package buffer-move
    :defer t))

(defun yxl-workspace/init-eyebrowse ()
  (use-package eyebrowse
    :ensure t
    :init
    (progn
      (setq eyebrowse-keymap-prefix (kbd "C-c w"))
      (eyebrowse-mode))
    :config
    (progn
      (setq eyebrowse-wrap-around t)
      (setq eyebrowse-mode-line-style 'always)
      (setq eyebrowse-new-workspace 'dired-stay-or-jump)
      (yxl-workspace/setup-eyebrowse)
      (eyebrowse-setup-opinionated-keys)
      ;; overwrite gc
      (define-key evil-motion-state-map "gc" 'evilnc-comment-operator)
      (yxl-workspace/setup-eyebrowse-keys)
      (add-to-list 'window-persistent-parameters '(window-side . writable))
      (add-to-list 'window-persistent-parameters '(window-slot . writable)))))

(defun yxl-workspace/init-yxl-session ()
  (use-package yxl-session
    :defer t
    :commands (yxl-session-load-1
               yxl-session-load-2
               yxl-session-save-1
               yxl-session-save-2)
    :config
    (progn
      (setq yxl-session-location "~/Dropbox/inbox/"))))
