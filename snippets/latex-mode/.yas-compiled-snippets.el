;;; Compiled snippets and support files for `latex-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'latex-mode
                     '(("tcap" "\\caption[${1:title}]{%\n  \\textbf{$1} \\\\\n  \\footnotesize{\n    ${0:descption}\n  }}\n" "table caption" nil nil nil "/Users/lysender61/dotfiles/.spacemacs.d/private/snippets/latex-mode/table-caption" nil nil)
                       ("m" "\\\\($1\\\\)\n" "math" nil nil nil "/Users/lysender61/dotfiles/.spacemacs.d/private/snippets/latex-mode/math" nil nil)
                       ("begin" "\\begin{${1:environment}}\n  `yas/selected-text`$0\n\\end{$1}" "\\begin{environment} ... \\end{environment}" nil nil nil "/Users/lysender61/dotfiles/.spacemacs.d/private/snippets/latex-mode/env" nil nil)
                       ("d" "\\\\[$1\\\\]\n" "disp-math" nil nil nil "/Users/lysender61/dotfiles/.spacemacs.d/private/snippets/latex-mode/disp-math" nil nil)))


;;; Do not edit! File generated at Wed Sep 14 16:46:50 2016
