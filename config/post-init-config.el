(with-eval-after-load 'helm
  ;; rebind C-z to C-tab, easier to press
  (define-key helm-map (kbd "C-<tab>") 'helm-select-action)
  (define-key helm-map (kbd "C-z") nil)
  (define-key helm-map (kbd "C-h") 'backward-delete-char))

(with-eval-after-load 'helm-files
  ;; overwrite spacemacs default
  (define-key helm-find-files-map (kbd "C-h") 'backward-delete-char)
  (define-key helm-read-file-map (kbd "C-h") 'backward-delete-char))

;; ivy views
(with-eval-after-load 'ivy
  (setq ivy-use-virtual-buffers nil)
  (add-to-list 'ivy-views
               '("{} init.el dotfiles"
                 (horz
                  (file "~/dotfiles/spacemacs.d/init.el" 7993)
                  (file "~/dotfiles/spacemacs.d" 7092)))
               t)
  (add-to-list 'ivy-views
               '("{} sec_052_res_debtrank.tex sections"
                 (horz
                  (file "~/Downloads/yxl105_ctw/yxl105_tex/phd_ch3_network/sections/sec_052_res_debtrank.tex" 6328)
                  (file "~/Downloads/yxl105_ctw/yxl105_tex/phd_ch3_network/sections/" 729)))
               t)
  (add-to-list 'ivy-views
               '("{} phd_ch3_network yxl105_c3_1607.tex"
                 (horz
                  (file "~/Downloads/yxl105_ctw/yxl105_tex/phd_ch3_network/yxl105_c3_1607.tex" 32)
                  (file "~/Downloads/yxl105_ctw/yxl105_tex/phd_ch3_network/" 1529)))
               t)
  )
