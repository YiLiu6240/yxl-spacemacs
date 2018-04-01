;; NOTE: These layers are not loaded
;;       - spacemacs-layouts (persp, eyebrowse)
;;       NOTE: counsel-projectile is loaded by spacemacs-layouts
;;             which should not have been
;;       - spacemacs-modeline (spaceline)
;;       TODO: enable spacemacs purpose
;;       - spacemacs-purpose
(configuration-layer/declare-layers '(spacemacs-completion
                                      spacemacs-defaults
                                      spacemacs-editing-visual
                                      spacemacs-editing
                                      spacemacs-evil
                                      spacemacs-language
                                      spacemacs-misc
                                      spacemacs-navigation
                                      spacemacs-org
                                      spacemacs-project
                                      ;; spacemacs-purpose
                                      spacemacs-visual))

;; REVIEW: verify upstream update
;; (configuration-layer/declare-layers '(spacemacs-completion
;;                                       spacemacs-editing
;;                                       spacemacs-editing-visual
;;                                       spacemacs-evil
;;                                       spacemacs-language
;;                                       spacemacs-misc
;;                                       (spacemacs-ui :packages (not doc-view))
;;                                       spacemacs-ui-visual
;;                                       spacemacs-org))
