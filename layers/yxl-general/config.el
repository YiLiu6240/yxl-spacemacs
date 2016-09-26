(setq elfeed-db-directory "~/Dropbox/rss/.elfeed")

(setq yxl-pdf-occur-preset
      '("previous\|traditional\|critici\|drawback"
        "hypothes\|propose\|question\|problem\|attempt\|issue\|approach\|method"
        "result\|stat\|result\|show\|support"
        "regulat\|supervis\|prudential\|systemic\|policy"
        "contagion\|spillover\|shock"))

(setq yxl-pdf-occur-preset-all
      (mapconcat 'identity yxl-pdf-occur-preset "\|"))

(setq yxl-helm-pdf-occur-sources-preset
      '((name . "pdf preset keywords")
        (candidates . yxl-pdf-occur-preset)
        (action . (("Search" . (lambda (x)
                                 (interactive)
                                 (pdf-occur x t)))))))

(setq yxl-helm-pdf-occur-sources-all
      `((name . "all keywords")
        (candidates . (("all" . yxl/pdf-occur-search-preset)))
        (action . (("open" . (lambda (x) (call-interactively x)))))))
