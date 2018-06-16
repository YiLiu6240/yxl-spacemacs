(defun yxl-text/highlight-todos ()
  ;; hl-todo-mode only highlights in comments
  ;; this is useful in main text
  (highlight-lines-matching-regexp
   "\\<\\(FIXME\\|BUG\\|NEXT\\):?"
   'hi-green)
  (highlight-lines-matching-regexp
   "\\<\\(WIP\\):?"
   'hi-yellow)
  (highlight-lines-matching-regexp
   "\\<\\(TODO\\):?"
   'hi-blue))

(defun yxl-text/latex-align-buffer ()
  "basically ggVG then align"
  (interactive)
  (evil-set-marker ?z)
  (evil-goto-first-line)
  (evil-visual-line)
  (evil-goto-line)
  ;; cant use paranthesis
  (call-interactively 'align)
  (evil-goto-mark ?z))

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
  (highlight-regexp "\\." 'yxl-latex-font-delim)
  ;; (highlight-regexp "_" 'yxl-latex-font-delim)
  ;; (highlight-regexp "\\\\_" 'font-lock-function-name-face)
  (highlight-regexp "\\\\(\\|\\\\)" 'yxl-latex-font-math-delim))

(defun yxl-text/setup-latex-general ()
  "my general latex settings"
  ;; extra line padding
  (add-hook 'LaTeX-mode-hook (lambda () (setq line-spacing 4)))
  (add-hook 'LaTeX-mode-hook (lambda () (setq yxl-line-width 100)))
  ;; (add-hook 'LaTeX-mode-hook #'visual-fill-column-mode)
  ;; (add-hook 'LaTeX-mode-hook #'fci-mode)
  ;; (add-hook 'LaTeX-mode-hook (lambda () (auto-fill-mode -1)) t)
  (setq TeX-auto-local ".auto")
  (setq TeX-style-local ".style")
  (setq-default font-latex-fontify-script nil)
  ;; RET to newline no indent; S-RET to newline post indent
  ;; (define-key LaTeX-mode-map (kbd "RET") 'electric-indent-just-newline)
  ;; (define-key LaTeX-mode-map (kbd "S-RET") 'newline)
  (setq-default TeX-newline-function 'reindent-then-newline-and-indent)
  ;; (setq TeX-newline-function 'newline)
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
          ("citep" (("citep" "{")) 'yxl-latex-font-hide 'declaration))))
          ;; ("shadow-comment" (("comment" "{")) 'yxl-latex-font-comment 'declaration)
          ;; ("shadow-hidden" (("hide" "{")) 'yxl-latex-font-hide 'declaration)


(defun yxl-text/setup-latex-pairs ()
  "smartparens and evil-surround"
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (dolist (key '((?z . ("``" . "''"))
                             (?\" . ("``" . "''"))))
                (add-to-list 'evil-surround-pairs-alist key))))

  (with-eval-after-load 'smartparens
    ;; REVIEW: why latex-mode work, but not LaTeX-mode ?
    (sp-with-modes '(tex-mode
                     plain-tex-mode
                     latex-mode
                     LaTeX-mode)
      (sp-local-pair "\\(" "\\)" :trigger "\\m ")
      (sp-local-pair "\\[" "\\]" :trigger "\\n ")
      (sp-local-pair "\\( " " \\)" :trigger "\\M ")
      (sp-local-pair "\\[ " " \\]" :trigger "\\N ")
      (sp-local-pair "$" "$"
                     :trigger "\\k "
                     :pre-handlers '(sp-latex-pre-slurp-handler)
                     :post-handlers '(sp-latex-insert-spaces-inside-pair)))))

(defun yxl-text/setup-latex-reftex ()
  ;; set master file using directory variables
  (setq-default TeX-master t)
  (setq-default reftex-toc-split-windows-horizontally nil)
  (setq-default reftex-toc-include-labels t)
  (setq-default reftex-idle-time 0)
  (setq-default reftex-ref-macro-prompt nil))

(defun yxl-text/markdown-insert-gfm-code-block (&optional lang closer)
  "Insert GFM code block for language LANG.
If LANG is nil, the language will be queried from user.  If a
region is active, wrap this region with the markup instead.  If
the region boundaries are not on empty lines, these are added
automatically in order to have the correct markup."
  (interactive
   (list (let ((completion-ignore-case nil))
           (condition-case nil
               (markdown-clean-language-string
                (completing-read
                 "Programming language: "
                 (markdown-gfm-get-corpus)
                 nil 'confirm (car markdown-gfm-used-languages)
                 'markdown-gfm-language-history))
             (quit "")))))
  (unless (string= lang "") (markdown-gfm-add-used-language lang))
  (when (and (eq closer nil) (> (length lang) 0))
    (setq lang (concat " " lang)))
  (if (markdown-use-region-p)
      (let ((b (region-beginning)) (e (region-end)))
        (goto-char e)
        ;; if we're on a blank line, don't newline, otherwise the ```
        ;; should go on its own line
        (unless (looking-back "\n" nil)
          (newline))
        (insert "```")
        (markdown-ensure-blank-line-after)
        (goto-char b)
        ;; if we're on a blank line, insert the quotes here, otherwise
        ;; add a new line first
        (unless (looking-at-p "\n")
          (newline)
          (forward-line -1))
        (markdown-ensure-blank-line-before)
        (insert "```" lang))
    (markdown-ensure-blank-line-before)
    (insert "```" lang "\n\n```")
    (markdown-ensure-blank-line-after)
    (forward-line -1)))

(defun yxl-text/insert-code-block ()
  (interactive)
  (markdown-insert-gfm-code-block ""))

(defun yxl-text/insert-r-block ()
  "Insert an r-chunk in markdown mode. Necessary due to interactions between polymode and yas snippet"
  (interactive)
  (yxl-text/markdown-insert-gfm-code-block "{r}" t))
