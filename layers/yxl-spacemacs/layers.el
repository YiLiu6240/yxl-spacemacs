(configuration-layer/declare-layers '(spacemacs-completion
                                      spacemacs-editing
                                      spacemacs-editing-visual
                                      spacemacs-evil
                                      spacemacs-language
                                      spacemacs-misc
                                      (spacemacs-ui :packages (not doc-view))
                                      spacemacs-ui-visual
                                      spacemacs-org))

;; If the user has not explicitly declared `helm' or `ivy'
;; and they are using the standard distribution, assume they
;; want `helm' completion.
;; FIXME: not working
;; (unless (or (configuration-layer/layer-usedp 'ivy)
;;             (configuration-layer/layer-usedp 'helm))
;;   (configuration-layer/declare-layers '(helm)))
