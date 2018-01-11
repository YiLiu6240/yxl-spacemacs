(defun yxl-org/setup-keybindings ()
  (define-key org-mode-map
    (kbd "C-M-o") #'yxl-org/insert-source-block)
  (define-key org-mode-map
    (kbd "C-M-S-o") #'yxl-org/insert-source-block-and-edit)
  (define-key org-read-date-minibuffer-local-map
    (kbd "M-K") (lambda () (interactive)
                  (org-eval-in-calendar '(calendar-backward-week 1))))
  (define-key org-read-date-minibuffer-local-map
    (kbd "M-J") (lambda () (interactive)
                  (org-eval-in-calendar '(calendar-forward-week 1))))
  (define-key org-read-date-minibuffer-local-map
    (kbd "M-H") (lambda () (interactive)
                  (org-eval-in-calendar '(calendar-backward-day 1))))
  (define-key org-read-date-minibuffer-local-map
    (kbd "M-L") (lambda () (interactive)
                  (org-eval-in-calendar '(calendar-forward-day 1))))
  (evil-define-key 'normal org-mode-map
    "t" 'org-todo
    "_" 'projectile-dired
    "gh" 'outline-up-heading
    "gp" 'outline-previous-heading
    "gj" (if (fboundp 'org-forward-same-level) ;to be backward compatible with older org version
             'org-forward-same-level
           'org-forward-heading-same-level)
    "gk" (if (fboundp 'org-backward-same-level)
             'org-backward-same-level
           'org-backward-heading-same-level)
    "gl" 'outline-next-visible-heading
    "T" (lambda ()
          (interactive)
          (evil-org-eol-call
           (lambda() (org-insert-todo-heading nil))))
    "o" 'evil-open-below
    "O" 'evil-open-above
    "$" 'org-end-of-line
    "^" 'org-beginning-of-line
    "<" 'org-metaleft
    ">" 'org-metaright
    (kbd "TAB") 'org-cycle)
  (mapc (lambda (state)
          (evil-define-key state org-mode-map
            (kbd "M-l") 'org-metaright
            (kbd "M-h") 'org-metaleft
            (kbd "M-k") 'org-metaup
            (kbd "M-j") 'org-metadown
            (kbd "M-L") 'org-shiftmetaright
            (kbd "M-H") 'org-shiftmetaleft
            (kbd "M-K") 'org-shiftmetaup
            (kbd "M-J") 'org-shiftmetadown
            (kbd "M-o") '(lambda () (interactive)
                           (evil-org-eol-call
                            '(lambda()
                               (org-insert-heading)
                               (org-metaright))))
            (kbd "M-t") '(lambda () (interactive)
                           (evil-org-eol-call
                            '(lambda()
                               (org-insert-todo-heading nil)
                               (org-metaright))))))
        '(normal insert)))

(defun yxl-org/setup-leader-keys ()
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "b" nil
    "m" nil)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "bi" #'yxl-org/insert-source-block
    "bb" #'org-edit-src-code
    "bn" #'org-next-block
    "bj" #'org-next-block
    "bp" #'org-previous-block
    "bk" #'org-previous-block
    "bs" #'org-babel-switch-to-session
    "bt" #'org-babel-result-hide-all
    "bT" #'yxl-org/babel-result-show-all
    "hi" #'org-insert-heading-respect-content
    "ht" #'org-insert-todo-heading-respect-content
    "hy" #'org-insert-todo-subheading
    "mm" #'org-toggle-latex-fragment
    "r" #'yxl-org-refile-visible
    "R" #'yxl-org/refile-to-scratch
    "." #'org-time-stamp
    "C-." #'yxl-org/general-hydra/body)
  (spacemacs/declare-prefix-for-mode 'org-mode "b" "src-block")
  (spacemacs/declare-prefix-for-mode 'org-mode "m" "math"))

(defun yxl-org/setup-hydra ()
  (defhydra yxl-org/general-hydra (:color red
                                          :pre (setq which-key-inhibit t)
                                          :post (setq which-key-inhibit nil))
    "
yxl-org/hydra:
--------
[_c_]: ?c? company-mode [_i_]: ?i? org-indent-mode
[_or_]: ?or? yxl-org/ob-R-helper-mode [_op_]: ?op? yxl-org/ob-python-helper-mode
[_oc_]: ?oc? yxl-org/ob-clojure-helper-mode
[_ss_]: ?ss? org-src-fontify-natively
--------
"
    ("." nil "quit")
    ("i" org-indent-mode
     (if (bound-and-true-p org-indent-mode) "[x]" "[ ]"))
    ("c" company-mode
     (if (bound-and-true-p company-mode) "[x]" "[ ]"))
    ("or" yxl-org/ob-R-helper-mode (if yxl-org/ob-R-helper-mode "[x]" "[ ]"))
    ("op" yxl-org/ob-ipython-helper-mode (if yxl-org/ob-ipython-helper-mode "[x]" "[ ]"))
    ("oc" yxl-org/ob-clojure-helper-mode (if yxl-org/ob-ipython-helper-mode "[x]" "[ ]"))
    ("ss" (lambda () (interactive)
            (yxl-org/toggle-org-src-fontify-natively) (revert-buffer nil t))
     (if org-src-fontify-natively "[x]" "[ ]"))))
