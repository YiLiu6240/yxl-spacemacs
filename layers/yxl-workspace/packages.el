(setq yxl-workspace-packages '((eyebrowse :location local)
                               markdown-mode))

(defun yxl-workspace/pre-init-markdown-mode ()
  (spacemacs|use-package-add-hook markdown-mode
    :post-config
    (progn
      (remove-hook 'markdown-mode-hook 'orgtbl-mode))))

(defun yxl-workspace/init-eyebrowse ()
  (use-package eyebrowse
    :commands (eyebrowse-mode)
    :init
    (progn
      (setq eyebrowse-wrap-around t)
      (setq eyebrowse-keymap-prefix (kbd "C-c w"))
      (eyebrowse-mode)

      (defun eyebrowse-create-window-config-clone ()
        (interactive)
        (let* ((eyebrowse-new-workspace nil))
          (call-interactively 'eyebrowse-create-window-config)
          (message "clone to new workspace")))
      ;; transient state
      (defun spacemacs//workspaces-ts-hint ()
        "Return a one liner string containing all the workspaces names."
        (concat
         " "
         (mapconcat 'spacemacs//workspace-format-name
                    (eyebrowse--get 'window-configs) " | ")
         (if (equal 1 spacemacs--ts-full-hint-toggle)
             spacemacs--workspaces-mod-ts-full-hint
           (concat "  (["
                   (propertize "?" 'face 'hydra-face-red)
                   "] help)"))))
      (spacemacs|transient-state-format-hint workspaces
        spacemacs--workspaces-ts-full-hint
        "\n\n
 Go to^^^^^^                             Actions^^
 ─────^^^^^^───────────────────────      ───────^^──────────────────────
 [_0_,_9_]^^     nth/new workspace       [_d_] close current workspace
 [_C-0_,_C-9_]^^ nth/new workspace       [_R_/_,_] rename current workspace
 [_<tab>_]^^^^   last workspace          [_c_/_C_] create window config
                                         [_?_] toggle help
 [_n_/_C-l_/_L_]^^   next workspace      [_q_] quit
 [_N_/_p_/_C-h_/_H_] prev workspace\n")

      (spacemacs|define-transient-state workspaces
      :title "Workspaces Transient State"
      :hint-is-doc t
      :dynamic-hint (spacemacs//workspaces-mod-ts-hint)
      :bindings
      ("q" nil)
      ("?" spacemacs//workspaces-ts-toggle-hint)
      ("0" eyebrowse-switch-to-window-config-0 :exit t)
      ("1" eyebrowse-switch-to-window-config-1 :exit t)
      ("2" eyebrowse-switch-to-window-config-2 :exit t)
      ("3" eyebrowse-switch-to-window-config-3 :exit t)
      ("4" eyebrowse-switch-to-window-config-4 :exit t)
      ("5" eyebrowse-switch-to-window-config-5 :exit t)
      ("6" eyebrowse-switch-to-window-config-6 :exit t)
      ("7" eyebrowse-switch-to-window-config-7 :exit t)
      ("8" eyebrowse-switch-to-window-config-8 :exit t)
      ("9" eyebrowse-switch-to-window-config-9 :exit t)
      ("C-0" eyebrowse-switch-to-window-config-0)
      ("C-1" eyebrowse-switch-to-window-config-1)
      ("C-2" eyebrowse-switch-to-window-config-2)
      ("C-3" eyebrowse-switch-to-window-config-3)
      ("C-4" eyebrowse-switch-to-window-config-4)
      ("C-5" eyebrowse-switch-to-window-config-5)
      ("C-6" eyebrowse-switch-to-window-config-6)
      ("C-7" eyebrowse-switch-to-window-config-7)
      ("C-8" eyebrowse-switch-to-window-config-8)
      ("C-9" eyebrowse-switch-to-window-config-9)
      ("<tab>" eyebrowse-last-window-config)
      ("C-i" eyebrowse-last-window-config)
      ("C-h" eyebrowse-prev-window-config)
      ("C-l" eyebrowse-next-window-config)
      ("H" eyebrowse-prev-window-config)
      ("L" eyebrowse-next-window-config)
      ("c" eyebrowse-create-window-config :exit t)
      ("C" eyebrowse-create-window-config-clone :exit t)
      ("d" eyebrowse-close-window-config)
      ("n" eyebrowse-next-window-config)
      ("N" eyebrowse-prev-window-config)
      ("p" eyebrowse-prev-window-config)
      ("R" spacemacs/workspaces-ts-rename :exit t)
      ("," spacemacs/workspaces-ts-rename :exit t))
      ;; note: we don't need to declare the `SPC l w' binding, it is
      ;; declare in the layout transient state
      ;; vim-style tab switching
      (define-key evil-motion-state-map "gt" 'eyebrowse-next-window-config)
      (define-key evil-motion-state-map "gT" 'eyebrowse-prev-window-config))
    :config
    (progn
      (add-to-list 'window-persistent-parameters '(window-side . writable))
      (add-to-list 'window-persistent-parameters '(window-slot . writable))
      (setq eyebrowse-new-workspace 'dired-jump)
      (setq eyebrowse-mode-line-style 'always))))
