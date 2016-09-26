(setq elfeed-db-directory "~/Dropbox/rss/.elfeed")

(setq yxl-pdf-occur-preset
      '("previous\|traditional\|critici\|drawback"
        "hypothes\|propose\|question\|problem\|attempt\|issue\|approach\|method"
        "result\|stat\|result\|show\|support"
        "regulat\|supervis\|prudential\|systemic\|policy"
        "contagion\|spillover\|shock"))

(setq yxl-pdf-occur-preset-all
      (mapconcat 'identity yxl-pdf-occur-preset "\|"))
