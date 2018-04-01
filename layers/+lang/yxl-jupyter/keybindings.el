(defun yxl-jupyter/setup-general-keybindings ()
  (with-eval-after-load 'ein-notebooklist
    (define-key ein:notebooklist-mode-map
      "o" 'spacemacs/ace-buffer-links))
  (with-eval-after-load 'ein-notebook
    (define-key ein:notebook-mode-map
      (kbd "C-c C-h") #'yxl-jupyter/help))
  (with-eval-after-load 'ein-multilang
    (evil-define-key 'insert ein:notebook-multilang-mode-map
      (kbd "<C-return>") #'ein:worksheet-execute-cell
      (kbd "<S-return>") #'ein:worksheet-execute-cell-and-goto-next)
    (evil-define-key 'normal ein:notebook-multilang-mode-map
      ;; keybindings mirror ipython web interface behavior
      (kbd "<C-return>") #'ein:worksheet-execute-cell
      (kbd "<S-return>") #'ein:worksheet-execute-cell-and-goto-next
      "gj" #'ein:worksheet-goto-next-input
      "gk" #'ein:worksheet-goto-prev-input)
    (define-key ein:notebook-multilang-mode-map
      (kbd "M-j") #'ein:worksheet-move-cell-down)
    (define-key ein:notebook-multilang-mode-map
      (kbd "M-k") #'ein:worksheet-move-cell-up)
    (define-key ein:edit-cell-mode-map
      (kbd "<C-return>") #'ein:edit-cell-save-and-execute-and-exit)))

(defun yxl-jupyter/setup-evilified-keybindings ()
  (evilified-state-evilify-map ein:pager-mode-map
    :mode ein:pager-mode
    :eval-after-load ein-pager
    :bindings
    (kbd "q") #'quit-window)
  (evilified-state-evilify-map ein:notebooklist-mode-map
    :mode ein:notebooklist-mode
    :eval-after-load ein-notebooklist
    :bindings
    (kbd "O") #'spacemacs/ace-buffer-links))

(defun yxl-jupyter/setup-leader-keys ()
  (spacemacs/declare-prefix-for-mode 'ein:notebook-multilang-mode
    "mw" "worksheet")
  (spacemacs/declare-prefix-for-mode 'ein:notebook-multilang-mode
    "mc" "cell")
  (spacemacs/declare-prefix-for-mode 'ein:notebook-multilang-mode
    "mn" "notebook")
  (spacemacs/set-leader-keys-for-major-mode 'ein:notebook-multilang-mode
    "." #'yxl-jupyter/jupyter-hydra/body
    dotspacemacs-major-mode-leader-key
    #'ein:worksheet-execute-cell-and-goto-next
    "o" #'ein:worksheet-insert-cell-below
    "O" #'ein:worksheet-insert-cell-above
    "C-o" #'ein:worksheet-insert-cell-below-and-edit
    ;; cell-wise
    "cc" #'ein:edit-cell-contents
    "cd" #'ein:worksheet-kill-cell
    "cp" #'ein:worksheet-yank-cell
    "cu" #'ein:worksheet-change-cell-type
    "ct" #'ein:worksheet-toggle-output
    "cS" #'ein:worksheet-toggle-slide-type
    "c C-l" #'ein:worksheet-clear-output
    "c C-k" #'ein:worksheet-merge-cell
    "c C-j" #'spacemacs/ein:worksheet-merge-cell-next
    ;; notebook-wise
    "nx" #'ein:notebook-close
    "cy" #'ein:worksheet-copy-cell
    "nR" #'ein:notebook-rename-command
    "n S" #'ein:worksheet-toggle-slideshow-view
    "n C-s" #'ein:notebook-save-notebook-command
    "n C-l" #'ein:worksheet-clear-all-output
    "n C-v" #'ein:worksheet-collapse-output
    ;; worksheet-wise
    "w1" #'ein:notebook-worksheet-open-1th
    "w2" #'ein:notebook-worksheet-open-2th
    "w3" #'ein:notebook-worksheet-open-3th
    "w4" #'ein:notebook-worksheet-open-4th
    "w5" #'ein:notebook-worksheet-open-5th
    "w6" #'ein:notebook-worksheet-open-6th
    "w7" #'ein:notebook-worksheet-open-7th
    "w8" #'ein:notebook-worksheet-open-8th
    "w9" #'ein:notebook-worksheet-open-last
    "ws" #'ein:notebook-scratchsheet-open
    "w+" #'ein:notebook-worksheet-insert-next
    "w-" #'ein:notebook-worksheet-delete)
  ;; keybindings for ipython notebook traceback mode
  (spacemacs/set-leader-keys-for-major-mode 'ein:traceback-mode
    "RET" 'ein:tb-jump-to-source-at-point-command
    "n" 'ein:tb-next-item
    "p" 'ein:tb-prev-item
    "q" 'bury-buffer))

(defun yxl-jupyter/setup-hydra ()
  (defhydra yxl-jupyter/jupyter-hydra (:color blue :hint nil
                                                  :pre (setq which-key-inhibit t)
                                                  :post (setq which-key-inhibit nil))
    ("q" nil)
    ("h" ein:notebook-worksheet-open-prev-or-last "Prev worksheet")
    ("j" ein:worksheet-goto-next-input :color red "Next cell")
    ("k" ein:worksheet-goto-prev-input :color red "Prev cell")
    ("l" ein:notebook-worksheet-open-next-or-first "Next worksheet")
    ("H" ein:notebook-worksheet-move-prev "Nav prev worksheet")
    ("J" ein:worksheet-move-cell-down :color red "Move cell down")
    ("K" ein:worksheet-move-cell-up :color red "Move cell up")
    ("L" ein:notebook-worksheet-move-next "Nav next worksheet")))
