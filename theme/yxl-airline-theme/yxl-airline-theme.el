(deftheme yxl-airline
  "Designed for use with the base16 emacs color schemes in the gui

url: https://github.com/mkaito/base16-emacs")

;; based on gruvbox
(let* ((color-active-fg "#d5c4a1") ;; active modeline fg, should be dimmer than default fg
       (color-inactive-fg "#928374") ;; inactive modeline fg, even dimmer
       (color-bg "#32302f") ;; modeline bg, should be lighter than default bg
       (color-bg-alt "#504945") ;; modeline bg, a more "highlighted" version
       ;; color when the specified evil state is active
       (color-state-fg "#ebdbb2") ;; general state fg
       (color-normal-bg "#7c6f64")
       (color-insert-bg "#427b58")
       (color-visual-bg "#af3a03")
       (color-replace-bg "#cc241d")
       (color-emacs-bg "#d3869b")

       (normal-outer-foreground  color-state-fg) (normal-outer-background  color-normal-bg)
       (normal-inner-foreground  color-active-fg) (normal-inner-background  color-bg-alt)
       (normal-center-foreground color-active-fg) (normal-center-background color-bg)

       (insert-outer-foreground  color-state-fg) (insert-outer-background  color-insert-bg)
       (insert-inner-foreground  color-active-fg) (insert-inner-background  color-bg-alt)
       (insert-center-foreground color-active-fg) (insert-center-background color-bg)

       (visual-outer-foreground  color-state-fg) (visual-outer-background  color-visual-bg)
       (visual-inner-foreground  color-active-fg) (visual-inner-background  color-bg-alt)
       (visual-center-foreground color-active-fg) (visual-center-background color-bg)

       (replace-outer-foreground  color-state-fg) (replace-outer-background  color-replace-bg)
       (replace-inner-foreground  color-active-fg) (replace-inner-background  color-bg-alt)
       (replace-center-foreground color-active-fg) (replace-center-background color-bg)

       (emacs-outer-foreground  color-state-fg) (emacs-outer-background  color-emacs-bg)
       (emacs-inner-foreground  color-active-fg) (emacs-inner-background  color-bg-alt)
       (emacs-center-foreground color-active-fg) (emacs-center-background color-bg)

       (inactive1-foreground color-inactive-fg) (inactive1-background color-bg)
       (inactive2-foreground color-inactive-fg) (inactive2-background color-bg)
       (inactive3-foreground color-inactive-fg) (inactive3-background color-bg))

  (yxl-airline-themes-set-deftheme 'yxl-airline)

  (when airline-cursor-colors
    (progn
     (setq evil-emacs-state-cursor   emacs-outer-background)
     (setq evil-normal-state-cursor  normal-outer-background)
     (setq evil-insert-state-cursor  `(bar ,insert-outer-background))
     (setq evil-replace-state-cursor replace-outer-background)
     (setq evil-visual-state-cursor  visual-outer-background))))

(yxl-airline-themes-set-modeline)

(provide-theme 'yxl-airline)
;;; airline-base16-gui-dark-theme.el ends here
