(setq ov-highlighter-packages '(ov
                                (ov-highlighter :location local)
                                (org-ov-highlighter :location local)))

(defun ov-highlighter/init-ov ()
  (use-package ov
     :defer t))

(defun ov-highlighter/init-ov-highlighter ()
  (use-package ov-highlighter
    :init
    (progn
      (spacemacs/set-leader-keys
        "xh" #'ov-highlighter/body))))

;; TODO: org-ov-highlighter
