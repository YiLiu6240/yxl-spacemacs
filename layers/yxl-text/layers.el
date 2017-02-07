(configuration-layer/declare-layers
 '((markdown :variables markdown-live-preview-engine 'vmd)
   ;; markdown: need vmd from npm: npm install -g vmd
   pandoc
   (latex :variables
          ;; LatexMK has to be enabled to let spc fetch auctex-latexmk
          ;; latex-build-command "LatexMk"
          ;; NOTE: could not get latexmk to work properly, use latex
          latex-build-command "LaTeX"
          latex-enable-auto-fill nil
          latex-enable-folding t)
   bibtex))
