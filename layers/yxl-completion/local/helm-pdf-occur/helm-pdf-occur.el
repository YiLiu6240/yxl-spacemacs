;; provide a helm interface for searching in pdf-tools
;; Author: Yi Liu <https://github.com/YiLiu6240>

(require 'helm)
(require 'pdf-tools)



(setq helm-pdf-occur-preset-list
      '("previous\|traditional\|critici\|drawback"
        "hypothes\|propose\|question\|problem\|attempt\|issue\|approach\|method"
        "result\|stat\|result\|show\|support"
        "regulat\|supervis\|prudential\|systemic\|policy"
        "contagion\|spillover\|shock"))

(setq helm-pdf-occur-sources-preset
      '((name . "Preset Keywords")
        (candidates . helm-pdf-occur-preset-list)
        (action . (("Search" . (lambda (x)
                                 (interactive)
                                 (pdf-occur x t)))))))

(setq helm-pdf-occur-preset-all
      (mapconcat 'identity helm-pdf-occur-preset-list "\|"))

(setq helm-pdf-occur-sources-all
      `((name . "All Keywords")
        (candidates . (("Search all" . helm-pdf-occur-search-preset)))
        (action . (("open" . (lambda (x) (call-interactively x)))))))


(setq helm-pdf-occur-sources-fallback
      (helm-build-sync-source "Fallback"
                              :match (lambda (_candidate) t)
                              :candidates '("Search Pattern")
                              :action (lambda (cand)
                                        (pdf-occur helm-pattern t))))



(defun helm-pdf-occur-search-preset ()
  (interactive)
  (pdf-occur helm-pdf-occur-preset-all t))

(defun helm-pdf-occur ()
  "Three sources:
- `helm-pdf-occur-sources-preset':
  sources obtained from `helm-pdf-occur-preset-list', search each preset entry.
- `helm-pdf-occur-sources-all':
  search a combined list of all entries in `helm-pdf-occur-preset-list'.
- `helm-pdf-occur-sources-fallback':
  search patterns provided by user input
"
  (interactive)
  (helm :sources '(helm-pdf-occur-sources-preset
                   helm-pdf-occur-sources-all
                   helm-pdf-occur-sources-fallback)
        :buffer "*helm pdf occur"))



(provide 'helm-pdf-occur)
