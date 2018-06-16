(require 'lsp-mode)
(require 'lsp-common)

(lsp-define-stdio-client lsp-R "R"
                         (lambda () default-directory)
                         '("R" "--quiet" "--slave" "--no-init-file"
                           "-e" "languageserver::run()"))
(provide 'lsp-r)
