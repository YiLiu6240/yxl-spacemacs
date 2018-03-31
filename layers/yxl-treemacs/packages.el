;;; packages.el --- treemacs Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Alexander Miller <alexanderm@web.de>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst yxl-treemacs-packages
  '(treemacs
    treemacs-evil
    treemacs-projectile))

(defun yxl-treemacs/init-treemacs ()
  (use-package treemacs
    :commands (treemacs-select-window)
    :defer t
    :init
    (spacemacs/set-leader-keys
      "ft"    #'treemacs-toggle
      "fT"    #'treemacs
      "fB"    #'treemacs-bookmark
      "f C-t" #'treemacs-find-file)
    :config
    (progn
      ;; FIXME
      ;; (spacemacs/define-evil-state-face "treemacs" "MediumPurple1")
      (add-to-list 'evil-escape-excluded-states 'treemacs)
      (setq treemacs-follow-after-init t)
      (setq treemacs-width 35)
      (setq treemacs-position 'left)
      (setq treemacs-is-never-other-window nil)
      (setq treemacs-silent-refresh nil)
      (setq treemacs-indentation 2)
      (setq treemacs-change-root-without-asking nil)
      (setq treemacs-sorting 'alphabetic-desc)
      (setq treemacs-show-hidden-files t)
      (setq treemacs-never-persist nil)
      (setq treemacs-goto-tag-strategy 'refetch-index)
      (setq treemacs-collapse-dirs treemacs-use-collapsed-directories)
      (setq treemacs-no-png-images t)

      (when treemacs-use-follow-mode
        (treemacs-follow-mode t))

      (when treemacs-use-filewatch-mode
        (treemacs-filewatch-mode t))

      ;; this boundp check guards against a new feature that not all treemacs installations will have
      ;; TODO remove this guard in a few weeks
      (when (boundp 'treemacs-git-mode)
        (when (memq treemacs-use-git-mode '(simple extended))
          (treemacs-git-mode treemacs-use-git-mode))))))

(defun yxl-treemacs/init-treemacs-evil ()
  (use-package treemacs-evil
    :after treemacs))

(defun yxl-treemacs/init-treemacs-projectile ()
  (use-package treemacs-projectile
    :defer t
    :init
    (spacemacs/set-leader-keys
      "pt" #'treemacs-projectile-toggle)))
