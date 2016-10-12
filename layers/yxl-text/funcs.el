;; TODO: move the find related functions to yxl, or to new layer yxl-proj
(defun yxl-text/find-TeX-master ()
  "in order to set TeX-master, create a `.dir-locals.el' which contains
in the project root directory. "
  (interactive)
  ;; TODO: should detect if TeX-master is a string
  (if (not (eq t TeX-master))
      (find-file TeX-master)
    (message "TeX-master not set")))

(defun yxl-text/find-project-outline ()
  (interactive)
  (if (not (eq nil yxl-text-outline-file))
      (find-file yxl-text-outline-file)
    (yxl-text/find-TeX-master)))

(defun yxl-text/find-project-root ()
  "goto these location in order:
master-dir, projectile-project-root, ~/Downloads"
  (interactive)
  (if (not (eq nil master-dir))
      (find-file master-dir)
    (if (not (eq nil (projectile-project-root)))
        (find-file (projectile-project-root))
      (find-file yxl/Downloads))))

(defun yxl-text/latex-align-buffer ()
  "basically ggVG then align"
  (interactive)
  (evil-goto-first-line)
  (evil-visual-line)
  (evil-goto-line)
  ;; cant use paranthesis
  (call-interactively 'align))

(defface yxl-latex-font-hide
  '((t (:foreground "#4b798a" :slant italic)))
  "should be visibly lighter than comments")
(defface yxl-latex-font-comment
  '((t (:foreground "#54a070" :slant italic)))
  "should be visibly lighter than comments")
(defface yxl-latex-font-delim
  '((t (:foreground "#b58900")))
  "should be visibly lighter than comments")
(defface yxl-latex-font-keyword
  '((t (:foreground "#859900")))
  "should be visibly lighter than comments")
(defface yxl-latex-font-math-delim
  '((t (:foreground "#586e75")))
  "should be visibly lighter than comments")

(defun yxl-text/latex-hi-lock ()
  (interactive)
  (highlight-regexp "foobar\\|lorem" 'yxl-latex-font-keyword)
  (highlight-regexp "\\." 'yxl-latex-font-delim)
  (highlight-regexp "_" 'yxl-latex-font-delim)
  (highlight-regexp "\\\\_" 'font-lock-function-name-face)
  (highlight-regexp "\\\\(\\|\\\\)" 'yxl-latex-font-math-delim))

(defun yxl-text/setup-latex-general ()
  "my general latex settings"
  ;; extra line padding
  (add-hook 'LaTeX-mode-hook (lambda () (setq line-spacing 4)))
  (add-hook 'LaTeX-mode-hook (lambda () (setq yxl-line-width 100)))
  (remove-hook 'TeX-mode-hook #'auto-fill-mode)
  (setq-default font-latex-fontify-script nil)
  (setq-default TeX-newline-function 'reindent-then-newline-and-indent)
  (setq LaTeX-fill-excluded-macros '("hide" "comment"))
  (setq latex-noindent-environments '("document" "equation" "equation*"
                                      "align" "align*"))
  ;; latex section hierachy:
  ;; 0 - part; 1 - chapter; 2 - section; 3 - subsection; 4 - subsubsection;
  ;; 5 - paragraph; 6 - subparagraph
  (setq TeX-outline-extra '(("^%% " 2)
                            ("^%%% " 3)
                            ("^%%%% " 4)
                            ("^%%%%% " 5))))

(defun yxl-text/setup-latex-custom ()
  "custom auctex settings"
  (setq font-latex-user-keyword-classes
        '(("citet" (("citet" "{")) 'yxl-latex-font-hide 'declaration)
          ("citep" (("citep" "{")) 'yxl-latex-font-hide 'declaration)
          ("shadow-comment" (("comment" "{")) 'yxl-latex-font-comment 'declaration)
          ("shadow-hidden" (("hide" "{")) 'yxl-latex-font-hide 'declaration))))

(defun yxl-text/setup-latex-pairs ()
  "smartparens and evil-surround"
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (push '(?z . ("``" . "''")) evil-surround-pairs-alist)
              (push '(?\" . ("``" . "''")) evil-surround-pairs-alist)))

  (with-eval-after-load 'smartparens
    ;; REVIEW: why latex-mode work, but not LaTeX-mode ?
    (sp-local-pair 'latex-mode "\\(" "\\)" :trigger "\\m ")
    (sp-local-pair 'latex-mode "\\[" "\\]" :trigger "\\n ")
    (sp-local-pair 'latex-mode "\\( " " \\)" :trigger "\\M ")
    (sp-local-pair 'latex-mode "\\[ " " \\]" :trigger "\\N ")))

(defun yxl-text/setup-latex-reftex ()
  ;; set master file using directory variables
  (setq-default TeX-master t)
  (setq-default reftex-toc-split-windows-horizontally nil)
  (setq-default reftex-toc-include-labels t)
  (setq-default reftex-idle-time 0)
  (setq-default reftex-ref-macro-prompt nil))
