;;; Compiled snippets and support files for `latex-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'latex-mode
                     '(("tcap" "\\caption[${1:title}]{%\n  \\textbf{$1} \\\\\n  \\footnotesize{\n    ${2:descption}\n  }}\n" "table caption" nil nil nil "/home/yiliu/.spacemacs.d/snippets/latex-mode/table_caption" nil nil)
                       ("s" "_{$1}^{$2}\n" "subscript and superscript" nil nil nil "/home/yiliu/.spacemacs.d/snippets/latex-mode/math_suffix_sub-super" "C-c C-/" nil)
                       ("mm" "\\\\($0\\\\)\n" "math" nil nil nil "/home/yiliu/.spacemacs.d/snippets/latex-mode/math_plain" nil nil)
                       ("md" "\\\\[$1\\\\]\n" "disp-math" nil nil nil "/home/yiliu/.spacemacs.d/snippets/latex-mode/math_display" nil nil)
                       ("covar-math" "\\\\(CoVaR_{q}^{j|i}\\\\)\n" "CoVaR in math mode" nil nil nil "/home/yiliu/.spacemacs.d/snippets/latex-mode/exp_covar_math" nil nil)
                       ("dcovar-math" "\\\\(\\\\Delta CoVaR_{q}^{j|i}\\\\)\n" "Delta CoVaR in math mode" nil nil nil "/home/yiliu/.spacemacs.d/snippets/latex-mode/exp_covar_delta_math" nil nil)
                       ("dcovar" "\\\\Delta CoVaR_{q}^{j|i}" "Delta CoVaR" nil nil nil "/home/yiliu/.spacemacs.d/snippets/latex-mode/exp_covar_delta" nil nil)
                       ("covar" "CoVaR_{q}^{j|i}" "CoVaR" nil nil nil "/home/yiliu/.spacemacs.d/snippets/latex-mode/exp_covar" nil nil)
                       ("begin" "\\begin{${1:environment}}\n  $0\n\\end{$1}\n" "\\begin{environment} ... \\end{environment}" nil nil nil "/home/yiliu/.spacemacs.d/snippets/latex-mode/env_general" nil nil)
                       ("align" "\\begin{align}\n  $0\n\\end{align}\n" "\\begin{align} ... \\end{align}" nil nil nil "/home/yiliu/.spacemacs.d/snippets/latex-mode/env_align" nil nil)))


;;; Do not edit! File generated at Sun Nov  6 20:40:31 2016
