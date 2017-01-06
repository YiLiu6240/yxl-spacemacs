(setq browse-url-generic-program "open")
;; url: force using google-chrome in linux
(if (eq system-type 'gnu/linux)
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "google-chrome"))
