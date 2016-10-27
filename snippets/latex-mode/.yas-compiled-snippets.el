;;; Compiled snippets and support files for `latex-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'latex-mode
                     '(("tcap" "\\caption[${1:title}]{%\n  \\textbf{$1} \\\\\n  \\footnotesize{\n    ${0:descption}\n  }}\n" "table caption" nil nil nil "/Users/lysender61/.spacemacs.d/snippets/latex-mode/table-caption" nil nil)
                       ("m" "\\\\($1\\\\)\n" "math" nil nil nil "/Users/lysender61/.spacemacs.d/snippets/latex-mode/math" nil nil)
                       ("begin" "\\begin{${1:environment}}\n  `yas/selected-text`$0\n\\end{$1}" "\\begin{environment} ... \\end{environment}" nil nil nil "/Users/lysender61/.spacemacs.d/snippets/latex-mode/env" nil nil)
                       ("d" "\\\\[$1\\\\]\n" "disp-math" nil nil nil "/Users/lysender61/.spacemacs.d/snippets/latex-mode/disp-math" nil nil)
                       ("covar-math" "\\\\(CoVaR_{q}^{j|i}\\\\)\n" "CoVaR in math mode" nil nil nil "/Users/lysender61/.spacemacs.d/snippets/latex-mode/covar_math" nil nil)
                       ("dcovar-math" "\\\\(\\\\Delta CoVaR_{q}^{j|i}\\\\)\n" "Delta CoVaR in math mode" nil nil nil "/Users/lysender61/.spacemacs.d/snippets/latex-mode/covar_delta_math" nil nil)
                       ("dcovar" "\\\\Delta CoVaR_{q}^{j|i}" "Delta CoVaR" nil nil nil "/Users/lysender61/.spacemacs.d/snippets/latex-mode/covar_delta" nil nil)
                       ("covar" "CoVaR_{q}^{j|i}" "CoVaR" nil nil nil "/Users/lysender61/.spacemacs.d/snippets/latex-mode/covar" nil nil)))


;;; Do not edit! File generated at Thu Oct 27 12:22:53 2016
