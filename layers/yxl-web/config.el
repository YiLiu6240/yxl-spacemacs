;;; config.el --- W3M Layer configuration File for Spacemacs
;;
;; Original Author: Kuroi Mato <venmos@fuck.gfw.es>
;; URL: https://github.com/venmos/w3m-layer
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

;; url: force using google-chrome in linux
(if (eq system-type 'gnu/linux)
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "google-chrome"))
