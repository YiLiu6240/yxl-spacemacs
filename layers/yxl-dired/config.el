(with-eval-after-load 'dired-aux
  (add-to-list 'dired-compress-file-suffixes
               '("\\.zip\\'" ".zip" "unzip")))

(with-eval-after-load 'dired
  (define-key dired-mode-map "z" 'dired-zip-files))
