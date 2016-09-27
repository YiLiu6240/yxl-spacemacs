(setq yxl-text-packages '((text-mode :location built-in)
                          auctex
                          latex-extra
                          (bibtex :location built-in)
                          helm-bibtex
                          gscholar-bibtex
                          magic-latex-buffer
                          markdown-mode
                          writeroom-mode
                          writegood-mode
                          bing-dict
                          synonyms))

(defun yxl-text/init-text-mode ()
  (use-package text-mode
    :defer t
    :init
    (progn
      (add-hook 'text-mode-hook 'visual-line-mode)
      (add-hook 'text-mode-hook 'hl-todo-mode)
      (add-hook 'text-mode-hook (lambda ()
                                  (setq indent-tabs-mode nil
                                        tab-width 2)))
      (with-eval-after-load 'evil-surround
        (add-hook 'text-mode-hook 'yxl-text/evil-surround-pairs)))))

(defun yxl-text/post-init-auctex ()
  (with-eval-after-load 'latex
    (yxl-text/setup-latex-general)
    (yxl-text/setup-latex-custom)
    (yxl-text/setup-latex-pairs)
    (yxl-text/setup-latex-reftex)

    (spacemacs/declare-prefix-for-mode 'latex-mode "f" "fill")
    (spacemacs/set-leader-keys-for-major-mode 'latex-mode
      "ff" #'LaTeX-fill-region
      "fb" #'LaTeX-fill-buffer)))

(defun yxl-text/init-latex-extra ()
  (use-package latex-extra
    :defer t
    :init
    (progn
      (add-hook 'LaTeX-mode-hook #'latex-extra-mode))
    :config
    (progn
      (define-key latex-extra-mode-map (kbd "C-<tab>") #'latex/hide-show))))

(defun yxl-text/init-bibtex ()
  (use-package bibtex
    :defer t
    :init
    (progn
      (add-hook 'bibtex-mode-hook 'outline-minor-mode)
      (add-hook 'bibtex-mode-hook (lambda ()
                                    (setq outline-regexp
                                          "\\(%\\{2,3\\} \\)\\|\\([ \t]*@\\)")))
      (add-hook 'bibtex-mode-hook (lambda ()
                                    (defun outline-level ()
                                      (cond ((looking-at "%%% ") 1)
                                            ((looking-at "%% ") 2)
                                            ((looking-at "[ \t]*@") 3)
                                            (t 1000))))))))

(defun yxl-text/init-magic-latex-buffer ()
  (use-package magic-latex-buffer
    :defer t
    :commands (magic-latex-buffer)
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'latex-mode
        "om" #'magic-latex-buffer))))

(defun yxl-text/init-helm-bibtex ()
  (use-package helm-bibtex
    :defer t
    :init
    (progn
      (setq bibtex-completion-bibliography yxl/file-bib)
      (setq bibtex-completion-cite-prompt-for-optional-arguments nil)
      (setq bibtex-completion-cite-default-command "citet")
      (setq bibtex-completion-cite-default-as-initial-input nil)
      (setq bibtex-completion-notes-path "~/Dropbox/bib/bib_notes.org")
      (setq biblio-download-directory "~/Dropbox/bib/general")
      (setq bibtex-completion-library-path '("~/Dropbox/bib/general"
                                             "~/Dropbox/bib/topic_tmp"))

      (spacemacs/declare-prefix "oc" "cite")
      (spacemacs/set-leader-keys "occ" #'helm-bibtex))
    :config
    (progn
      (setq helm-bibtex-full-frame nil)
      ;; rearrange helm-bibtex actions
      (helm-delete-action-from-source "Insert citation" helm-source-bibtex)
      (helm-delete-action-from-source "Insert BibTeX key" helm-source-bibtex)
      (helm-delete-action-from-source "Insert reference" helm-source-bibtex)
      (helm-add-action-to-source
       "Insert citation" 'bibtex-completion-insert-citation helm-source-bibtex 0)
      (helm-add-action-to-source
       "Insert BibTeX key" 'bibtex-completion-insert-key helm-source-bibtex 1)
      (helm-add-action-to-source
       "Insert reference" 'bibtex-completion-insert-reference helm-source-bibtex 2)
      (setq bibtex-completion-format-citation-functions
            '((org-mode      . bibtex-completion-format-citation-org-link-to-PDF)
              (latex-mode    . bibtex-completion-format-citation-cite)
              (TeX-mode    . bibtex-completion-format-citation-cite)
              (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
              (default       . bibtex-completion-format-citation-default))))))

(defun yxl-text/init-gscholar-bibtex ()
  (use-package gscholar-bibtex
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys "ocg" #'gscholar-bibtex))
    :config
    (progn
      (setq gscholar-bibtex-default-source "Google Scholar")
      (gscholar-bibtex-source-on-off :off "IEEE Xplore")
      (gscholar-bibtex-source-on-off :off "ACM Digital Library")
      (gscholar-bibtex-source-on-off :off "DBLP")
      (setq gscholar-bibtex-database-file yxl/file-bib))))

(defun yxl-text/post-init-markdown-mode ()
  (when (configuration-layer/layer-usedp 'markdown)
    (setq auto-mode-alist (cons '("\\.text$" . gfm-mode) auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.md$" . gfm-mode) auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.mdown$" . gfm-mode) auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.mdt$" . gfm-mode) auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.markdown$" . gfm-mode) auto-mode-alist)))
  (with-eval-after-load 'markdown-mode
    (add-hook 'markdown-mode-hook 'outline-minor-mode)
    (add-hook 'markdown-mode-hook
              (lambda ()
                (setq markdown-command "pandoc")))))

(defun yxl-text/init-writeroom-mode ()
  (use-package writeroom-mode
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys "axW" 'writeroom-mode)
      (add-hook 'text-mode-hook
                (lambda ()
                  (setq writeroom-width 100)))
      (add-hook 'latex-mode-hook
                (lambda ()
                  (setq writeroom-width 100)))
      (setq-default writeroom-mode-line t)
      (setq-default writeroom-maximize-window nil))
    :config
    (progn
      (setq writeroom-global-effects
            (delq 'writeroom-set-fullscreen writeroom-global-effects)))))

(defun yxl-text/init-writegood-mode ()
  (use-package writegood-mode
    :defer t))

(defun yxl-text/init-bing-dict ()
  (use-package bing-dict
    :defer t
    :init
    (spacemacs/set-leader-keys "ocd" 'bing-dict-brief)))

(defun yxl-text/init-synonyms ()
  ;; REVIEW: why command "synonyms" not present?
  (use-package synonyms
    :defer t
    :init
    (progn
      (setq synonyms-file "~/Dropbox/dict/mthesaur.txt")
      (setq synonyms-cache-file "~/Dropbox/dict/mthesaur.txt.cache"))))
