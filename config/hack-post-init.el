;; override spacemacs popwin
(delete '("^\*WoMan.+\*$" :regexp t :position bottom)
      popwin:special-display-config)
;; (delete '("*Help*" :dedicated t :position bottom :stick t :noselect t :height 0.4)
;;       popwin:special-display-config)
