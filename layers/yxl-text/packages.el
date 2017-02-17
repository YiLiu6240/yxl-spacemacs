(setq yxl-text-packages '((text-mode :location built-in)
                          auctex
                          (bibtex :location built-in)
                          org-ref
                          gscholar-bibtex
                          magic-latex-buffer
                          markdown-mode
                          writegood-mode
                          bing-dict
                          synonyms
                          helm-dictionary))

(defun yxl-text/init-text-mode ()
  (use-package text-mode
    :defer t
    :init
    (progn
      (add-hook 'text-mode-hook 'visual-line-mode)
      (add-hook 'text-mode-hook 'hl-todo-mode)
      (add-hook 'text-mode-hook (lambda ()
                                  (setq-local tab-width 2)
                                  (setq-local evil-shift-width 2)))
      (add-hook 'markdown-mode-hook (lambda ()
                                      (setq-local tab-width 4)
                                      (setq-local evil-shift-width 4))))))

(defun yxl-text/post-init-auctex ()
  (with-eval-after-load 'latex
    ;; (add-hook 'LaTeX-mode-hook #'latex-extra-mode)
    (evil-set-initial-state 'reftex-toc-mode 'evilified)
    (evil-set-initial-state 'reftex-select-label-mode 'evilified)
    (yxl-text/setup-latex-general)
    (yxl-text/setup-latex-custom)
    (yxl-text/setup-latex-pairs)
    (yxl-text/setup-latex-reftex)
    (spacemacs/set-leader-keys-for-major-mode 'latex-mode
      "oa" #'yxl-text/latex-align-buffer)
    (add-hook 'LaTeX-mode-hook #'yxl-text/latex-hi-lock)
    (key-chord-define LaTeX-mode-map "__"
                      (lambda ()
                        (interactive)
                        (yas-expand-snippet "_{$1}^{$2}")))))

(defun yxl-text/init-bibtex ()
  (use-package bibtex
    :defer t
    :init
    (progn
      ;; HACK
      ;; TODO: should add text-mode hooks to bibtex-mode-hook
      (add-hook 'bibtex-mode-hook 'yxl-evil/evil-surround-pairs)
      (add-hook 'bibtex-mode-hook 'outline-minor-mode)
      (add-hook 'bibtex-mode-hook (lambda ()
                                    (setq outline-regexp
                                          "\\(%\\{2,3\\} \\)\\|\\([ \t]*@\\)")))
      (add-hook 'bibtex-mode-hook (lambda ()
                                    (defun outline-level ()
                                      (cond ((looking-at "%%% ") 1)
                                            ((looking-at "%% ") 2)
                                            ((looking-at "[ \t]*@") 3)
                                            (t 1000))))))
    :config
    (progn
      ;; override spacemacs config; already has `[['
      (evil-define-key 'normal bibtex-mode-map
           (kbd "C-j") 'evil-window-down
           (kbd "C-k") 'evil-window-up))))

(defun yxl-text/init-magic-latex-buffer ()
  (use-package magic-latex-buffer
    :defer t
    :commands (magic-latex-buffer)
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'latex-mode
        "om" #'magic-latex-buffer))))

(defun yxl-text/post-init-org-ref ()
  (with-eval-after-load 'helm-bibtex
    (setq org-ref-default-bibliography yxl-file-bib)
    (setq org-ref-pdf-directory nil)
    (setq org-ref-bibliography-notes "~/Dropbox/bib/bib_notes.org")
    (setq bibtex-completion-bibliography yxl-file-bib)
    (setq bibtex-completion-cite-prompt-for-optional-arguments nil)
    (add-to-list 'bibtex-completion-cite-commands "citet")
    (setq bibtex-completion-cite-default-command "citet")
    (setq bibtex-completion-cite-default-as-initial-input t)
    (setq bibtex-completion-notes-path "~/Dropbox/bib/bib_notes.org")
    (setq biblio-download-directory "~/Dropbox/bib/general")
    (setq bibtex-completion-library-path '("~/Dropbox/bib/general"
                                           "~/Dropbox/bib/topic_tmp"))
    (setq helm-bibtex-full-frame nil)
    (setq bibtex-completion-additional-search-fields '(keywords author title year journal chapter booktitle))
    (setq bibtex-completion-display-formats
          '((article . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:25}  ${title:*}  ${journal:25}")
            (inbook . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:25}  ${title:*}  Chapter ${chapter:25}")
            (incollection . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:25}  ${title:*}  ${booktitle:25}")
            (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:25}  ${title:*}  ${booktitle:25}")
            (t . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:25}  ${title:*}")))))

(defun yxl-text/init-gscholar-bibtex ()
  (use-package gscholar-bibtex
    :defer t
    :config
    (progn
      (setq gscholar-bibtex-default-source "Google Scholar")
      (gscholar-bibtex-source-on-off :off "IEEE Xplore")
      (gscholar-bibtex-source-on-off :off "ACM Digital Library")
      (gscholar-bibtex-source-on-off :off "DBLP")
      (setq gscholar-bibtex-database-file yxl-file-bib))))

(defun yxl-text/post-init-markdown-mode ()
  (when (configuration-layer/layer-usedp 'markdown)
    ;; (setq auto-mode-alist (cons '("\\.text$" . gfm-mode) auto-mode-alist))
    ;; (setq auto-mode-alist (cons '("\\.md$" . gfm-mode) auto-mode-alist))
    ;; (setq auto-mode-alist (cons '("\\.mdown$" . gfm-mode) auto-mode-alist))
    ;; (setq auto-mode-alist (cons '("\\.mdt$" . gfm-mode) auto-mode-alist))
    ;; (setq auto-mode-alist (cons '("\\.markdown$" . gfm-mode) auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.text$" . markdown-mode) auto-mode-alist)))
  (with-eval-after-load 'markdown-mode
    (add-hook 'markdown-mode-hook 'outline-minor-mode)
    (add-hook 'markdown-mode-hook
              (lambda ()
                (setq markdown-command "pandoc")))
    (setq markdown-asymmetric-header t)))

(defun yxl-text/init-writegood-mode ()
  (use-package writegood-mode
    :defer t))

(defun yxl-text/init-bing-dict ()
  (use-package bing-dict
    :defer t))

(defun yxl-text/init-synonyms ()
  ;; REVIEW: why command "synonyms" not present?
  (use-package synonyms
    :defer t
    :init
    (progn
      (setq synonyms-file "~/Dropbox/dict/mthesaur.txt")
      (setq synonyms-cache-file "~/Dropbox/dict/mthesaur.txt.cache"))))

(defun yxl-text/init-helm-dictionary ()
  (use-package helm-dictionary
    :defer t
    ;; TODO: needs an offline dictionary
    ;; TODO: configure online dictionaries for own preference
    :config
    (progn
      (setq helm-dictionary-database "~/Dropbox/dict/english-chinese.xdxf"))))
