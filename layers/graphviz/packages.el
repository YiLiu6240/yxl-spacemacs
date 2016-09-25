;;; packages.el --- graphviz layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst graphviz-packages
  '((graphviz-dot-mode :location local)))

(defun graphviz/init-graphviz-dot-mode ()
  (use-package graphviz-dot-mode
    :mode "\\.dot$"
    :defer t
    :config
    (progn
      (spacemacs|add-toggle graphviz-live-reload
        :status graphviz-dot-auto-preview-on-save
        :on (graphviz-turn-on-live-preview)
        :off (graphviz-turn-off-live-preview)
        :documentation "Enable Graphviz live reload.")
      (define-key graphviz-dot-mode-map (kbd "M-q") 'graphviz-dot-indent-graph)
      (spacemacs/set-leader-keys-for-major-mode 'graphviz-dot-mode
        "t" 'spacemacs/toggle-graphviz-live-reload
        "c" 'compile
        "p" 'graphviz-dot-preview
        "," 'graphviz-dot-preview))))

;;; packages.el ends here
