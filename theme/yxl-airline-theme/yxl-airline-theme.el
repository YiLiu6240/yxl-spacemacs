(deftheme yxl-airline
  "Designed for use with the base16 emacs color schemes in the gui

url: https://github.com/mkaito/base16-emacs")

(defun yxl-airline-theme-set-colors ()
  ;; based on gruvbox
  ;; TODO: set these colors according to the current theme fg/bg colors?
  (let* ((light-p (eq frame-background-mode 'light))
         ;; active modeline fg, should be dimmer than default fg
         (color-active-fg (if light-p "#504945" "#d5c4a1"))
         ;; inactive modeline fg, even dimmer
         (color-inactive-fg (if light-p "#928374" "#928374"))
         ;; modeline bg, should be lighter than default bg
         (color-bg (if light-p "#bdae93" "#32302f"))
         ;; modeline bg, a more "highlighted" version
         (color-bg-alt (if light-p "#928374" "#504945"))
         ;; border color for the modeline
         (color-border (if light-p "#504945" "#3c3836"))
         ;; color when the specified evil state is active
         ;; general state fg
         (color-state-fg (if light-p "#ebdbb2" "#ebdbb2"))
         (color-normal-bg (if light-p "#7c6f64" "#7c6f64"))
         ;; principle:
         ;; evilified should have a slightly lighter bg than normal bg in dark scheme
         ;;            darker bg in light scheme
         (color-evilified-bg (if light-p "#504945" "#a89984"))
         (color-insert-bg (if light-p "#427b58" "#427b58"))
         (color-visual-bg (if light-p "#af3a03" "#af3a03"))
         (color-replace-bg (if light-p "#cc241d" "#cc241d"))
         (color-emacs-bg (if light-p "#8f3f71" "#8f3f71"))

         (normal-outer-foreground  color-state-fg) (normal-outer-background  color-normal-bg)
         (normal-inner-foreground  color-active-fg) (normal-inner-background  color-bg-alt)
         (normal-center-foreground color-active-fg) (normal-center-background color-bg)

         (insert-outer-foreground  color-state-fg) (insert-outer-background  color-insert-bg)
         (insert-inner-foreground  color-active-fg) (insert-inner-background  color-insert-bg)
         (insert-center-foreground color-active-fg) (insert-center-background color-insert-bg)

         (visual-outer-foreground  color-state-fg) (visual-outer-background  color-visual-bg)
         (visual-inner-foreground  color-active-fg) (visual-inner-background  color-visual-bg)
         (visual-center-foreground color-active-fg) (visual-center-background color-visual-bg)

         (replace-outer-foreground  color-state-fg) (replace-outer-background  color-replace-bg)
         (replace-inner-foreground  color-active-fg) (replace-inner-background  color-replace-bg)
         (replace-center-foreground color-active-fg) (replace-center-background color-replace-bg)

         (emacs-outer-foreground  color-state-fg) (emacs-outer-background  color-emacs-bg)
         (emacs-inner-foreground  color-active-fg) (emacs-inner-background  color-emacs-bg)
         (emacs-center-foreground color-active-fg) (emacs-center-background color-emacs-bg)

         (inactive1-foreground color-inactive-fg) (inactive1-background color-bg)
         (inactive2-foreground color-inactive-fg) (inactive2-background color-bg)
         (inactive3-foreground color-inactive-fg) (inactive3-background color-bg))

    (yxl-airline-themes-set-deftheme 'yxl-airline)

    (when airline-cursor-colors
      (progn
        (setq evil-emacs-state-cursor   emacs-outer-background)
        (setq evil-normal-state-cursor  normal-outer-background)
        (setq evil-evilified-state-cursor  color-evilified-bg)
        (setq evil-insert-state-cursor  `(bar ,insert-outer-background))
        (setq evil-replace-state-cursor replace-outer-background)
        (setq evil-visual-state-cursor  visual-outer-background)))))

(yxl-airline-theme-set-colors)
(yxl-airline-themes-set-modeline)

(provide-theme 'yxl-airline)
;;; airline-base16-gui-dark-theme.el ends here
