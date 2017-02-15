(configuration-layer/declare-layers
 '((markdown :variables markdown-live-preview-engine 'vmd)
   ;; markdown: need vmd from npm: npm install -g vmd
   pandoc
   (latex :variables
          latex-build-command "LatexMk"
          latex-enable-auto-fill nil
          latex-enable-folding t)
   bibtex))
