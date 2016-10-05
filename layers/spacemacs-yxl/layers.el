(configuration-layer/declare-layers '(spacemacs-completion
                                      spacemacs-editing
                                      spacemacs-editing-visual
                                      spacemacs-evil
                                      spacemacs-language
                                      spacemacs-misc
                                      spacemacs-ui
                                      spacemacs-ui-visual
                                      spacemacs-org))
;; If the user has not explicitly declared `helm' or `ivy'
;; and they are using the standard distribution, assume they
;; want `helm' completion.
(unless (or (configuration-layer/layer-usedp 'ivy)
            (configuration-layer/layer-usedp 'helm))
  (configuration-layer/declare-layers '(helm)))
