(defun pdf-view-refresh-midnight-colors ()
  "get the current foreground color and background color, and set
`pdf-view-midnight-colors' accordingly"
  (interactive)
  (let* ((fg (face-attribute 'default :foreground))
         (bg (face-attribute 'default :background)))
    (setq pdf-view-midnight-colors `(,fg . ,bg))))

;; TODO: refine these functions
(define-minor-mode pdf-view-darknight-minor-mode
  "Apply a color-filter appropriate for past midnight reading.
The colors are determined by the variable
`pdf-view-midnight-colors', which see. "

  nil " Mid" nil
  (pdf-util-assert-pdf-buffer)
  (let ((enable (lambda ()
                  (pdf-info-setoptions
                   :render/foreground "#839496"
                   :render/background "#182c33"
                   :render/usecolors t))))
    (cond
     (pdf-view-darknight-minor-mode
      (add-hook 'after-save-hook enable nil t)
      (add-hook 'after-revert-hook enable nil t)
      (funcall enable))
     (t
      (remove-hook 'after-save-hook enable t)
      (remove-hook 'after-revert-hook enable t)
      (pdf-info-setoptions :render/usecolors nil))))
  (pdf-cache-clear-images)
  (pdf-view-redisplay t))

(define-minor-mode pdf-view-midday-minor-mode
  "Apply a color-filter appropriate for past midnight reading.
The colors are determined by the variable
`pdf-view-midnight-colors', which see. "

  nil " Mid" nil
  (pdf-util-assert-pdf-buffer)
  (let ((enable (lambda ()
                  (pdf-info-setoptions
                   :render/foreground "#52676f"
                   :render/background "#fcf4dc"
                   :render/usecolors t))))
    (cond
     (pdf-view-midday-minor-mode
      (add-hook 'after-save-hook enable nil t)
      (add-hook 'after-revert-hook enable nil t)
      (funcall enable))
     (t
      (remove-hook 'after-save-hook enable t)
      (remove-hook 'after-revert-hook enable t)
      (pdf-info-setoptions :render/usecolors nil))))
  (pdf-cache-clear-images)
  (pdf-view-redisplay t))

(defun yxl-pdf-view-bindings ()
  (interactive)  ; needs to call interactively due to bugs in pdf mode
  (evil-set-initial-state 'pdf-view-mode 'evilified)
  (evilified-state-evilify pdf-view-mode pdf-view-mode-map
    "-" #'dired-jump
    "_" #'pdf-view-shrink
    "+" #'pdf-view-enlarge
    "gg" #'yxl-pdf-view-goto-first-page
    "G" #'yxl-pdf-view-goto-page
    "e" #'pdf-view-scroll-down-or-previous-page
    "d" #'pdf-view-scroll-up-or-next-page
    "j"  #'pdf-view-next-line-or-next-page
    "k"  #'pdf-view-previous-line-or-previous-page
    "n" #'pdf-view-next-page
    "p" #'pdf-view-previous-page
    "J" #'pdf-view-next-page
    "K" #'pdf-view-previous-page
    "l"  #'image-forward-hscroll
    "h"  #'image-backward-hscroll)
  (spacemacs/set-leader-keys-for-major-mode #'pdf-view-mode
    "=" #'pdf-view-enlarge
    "-" #'pdf-view-shrink
    "gg" #'yxl-pdf-view-goto-first-page
    "G" #'yxl-pdf-view-goto-page
    "os" #'yxl-helm-pdf-occur
    "oS" #'yxl-pdf-occur-all-keywords
    "n" #'pdf-view-midnight-minor-mode
    "N" #'pdf-view-darknight-minor-mode
    "d" #'pdf-view-midday-minor-mode)
  (spacemacs|define-transient-state pdf-tools
    :title "PDF-tools transient state"
    :on-enter (setq which-key-inhibit t)
    :on-exit (setq which-key-inhibit nil)
    :evil-leader-for-mode (pdf-view-mode . ".")
    :doc
    "
 Navigation^^^^                Scale/Fit^^                    Annotations^^       Actions^^           Other^^
 ----------^^^^--------------- ---------^^------------------  -----------^^------ -------^^---------- -----^^---
 [_j_/_k_] scroll down/up      [_W_] fit to width             [_al_] list         [_s_] search         [_=_] enlarge
 [_h_/_l_] scroll left/right   [_H_] fit to height            [_at_] text         [_O_] outline        [_-_] shrink
 [_d_/_u_] pg down/up          [_P_] fit to page              [_aD_] delete       [_p_] print          [_q_] quit
 [_J_/_K_] next/prev pg        [_m_] slice using mouse        [_am_] markup       [_o_] open link
 [_0_/_$_] full scroll l/r     [_b_] slice from bounding box  ^^                  [_r_] revert
 ^^^^                          [_R_] reset slice              ^^                  [_t_] attachments
 ^^^^                          [_zr_] reset zoom              ^^                  [_n_] night mode
 "
    :bindings
    ;; Navigation
    ("j"  pdf-view-next-line-or-next-page)
    ("k"  pdf-view-previous-line-or-previous-page)
    ("l"  image-forward-hscroll)
    ("h"  image-backward-hscroll)
    ("J"  pdf-view-next-page)
    ("K"  pdf-view-previous-page)
    ("u"  pdf-view-scroll-down-or-previous-page)
    ("d"  pdf-view-scroll-up-or-next-page)
    ("0"  image-bol)
    ("$"  image-eol)
    ;; zoom
    ("-" pdf-view-shrink)
    ("=" pdf-view-enlarge)
    ;; Scale/Fit
    ("W"  pdf-view-fit-width-to-window)
    ("H"  pdf-view-fit-height-to-window)
    ("P"  pdf-view-fit-page-to-window)
    ("m"  pdf-view-set-slice-using-mouse)
    ("b"  pdf-view-set-slice-from-bounding-box)
    ("R"  pdf-view-reset-slice)
    ("zr" pdf-view-scale-reset)
    ;; Annotations
    ("aD" pdf-annot-delete)
    ("at" pdf-annot-attachment-dired :exit t)
    ("al" pdf-annot-list-annotations :exit t)
    ("am" pdf-annot-add-markup-annotation)
    ;; Actions
    ("s" pdf-occur :exit t)
    ("O" pdf-outline :exit t)
    ("p" pdf-misc-print-document :exit t)
    ("o" pdf-links-action-perform :exit t)
    ("r" pdf-view-revert-buffer)
    ("t" pdf-annot-attachment-dired :exit t)
    ("n" pdf-view-midnight-minor-mode)
    ;; Other
    ("q" nil :exit t)))

(defun yxl-pdf-view-goto-page ()
  "vim-style wrapper for pdf-view-goto-page. accepts G or 5G."
  (interactive)
  (if current-prefix-arg
      (pdf-view-goto-page (prefix-numeric-value current-prefix-arg))
    (pdf-view-last-page)))

(defun yxl-pdf-view-goto-first-page ()
  "vim-style wrapper for pdf-view-goto-page. accepts gg or 5gg."
  (interactive)
  (if current-prefix-arg
      (pdf-view-goto-page (prefix-numeric-value current-prefix-arg))
    (pdf-view-first-page)))

(defun yxl-dired-delete-window ()
  "delete the current dired window, if it is the only window, return to
spacemacs home buffer"
  (interactive)
  (if (one-window-p)
      (spacemacs/home)
    (delete-window)))

(defun yxl-config/setup-ibuffer-bindings ()
  ;; https://github.com/abo-abo/hydra/wiki/Ibuffer
  (defhydra hydra-ibuffer-main (:color pink :hint nil)
    "
   ^Navigation^ | ^Mark^        | ^Actions^        | ^View^
  -^----------^-+-^----^--------+-^-------^--------+-^----^-------
    _k_:    ʌ   | _m_: mark     | _D_: delete      | _g_: refresh
   _RET_: visit | _u_: unmark   | _S_: save        | _s_: sort
    _j_:    v   | _*_: specific | _a_: all actions | _/_: filter
  -^----------^-+-^----^--------+-^-------^--------+-^----^-------
  "
    ("j" ibuffer-forward-line)
    ("RET" ibuffer-visit-buffer :color blue)
    ("k" ibuffer-backward-line)

    ("m" ibuffer-mark-forward)
    ("u" ibuffer-unmark-forward)
    ("*" hydra-ibuffer-mark/body :color blue)

    ("D" ibuffer-do-delete)
    ("S" ibuffer-do-save)
    ("a" hydra-ibuffer-action/body :color blue)

    ("g" ibuffer-update)
    ("s" hydra-ibuffer-sort/body :color blue)
    ("/" hydra-ibuffer-filter/body :color blue)

    ("o" ibuffer-visit-buffer-other-window "other window" :color blue)
    ("q" ibuffer-quit "quit ibuffer" :color blue)
    ("." nil "toggle hydra" :color blue))
  (defhydra hydra-ibuffer-mark (:color teal :columns 5
                                       :after-exit (hydra-ibuffer-main/body))
    "Mark"
    ("*" ibuffer-unmark-all "unmark all")
    ("M" ibuffer-mark-by-mode "mode")
    ("m" ibuffer-mark-modified-buffers "modified")
    ("u" ibuffer-mark-unsaved-buffers "unsaved")
    ("s" ibuffer-mark-special-buffers "special")
    ("r" ibuffer-mark-read-only-buffers "read-only")
    ("/" ibuffer-mark-dired-buffers "dired")
    ("e" ibuffer-mark-dissociated-buffers "dissociated")
    ("h" ibuffer-mark-help-buffers "help")
    ("z" ibuffer-mark-compressed-file-buffers "compressed")
    ("b" hydra-ibuffer-main/body "back" :color blue))
  (defhydra hydra-ibuffer-action (:color teal :columns 4
                                         :after-exit
                                         (if (eq major-mode 'ibuffer-mode)
                                             (hydra-ibuffer-main/body)))
    "Action"
    ("A" ibuffer-do-view "view")
    ("E" ibuffer-do-eval "eval")
    ("F" ibuffer-do-shell-command-file "shell-command-file")
    ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
    ("H" ibuffer-do-view-other-frame "view-other-frame")
    ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
    ("M" ibuffer-do-toggle-modified "toggle-modified")
    ("O" ibuffer-do-occur "occur")
    ("P" ibuffer-do-print "print")
    ("Q" ibuffer-do-query-replace "query-replace")
    ("R" ibuffer-do-rename-uniquely "rename-uniquely")
    ("T" ibuffer-do-toggle-read-only "toggle-read-only")
    ("U" ibuffer-do-replace-regexp "replace-regexp")
    ("V" ibuffer-do-revert "revert")
    ("W" ibuffer-do-view-and-eval "view-and-eval")
    ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
    ("b" nil "back"))
  (defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
    "Sort"
    ("i" ibuffer-invert-sorting "invert")
    ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
    ("v" ibuffer-do-sort-by-recency "recently used")
    ("s" ibuffer-do-sort-by-size "size")
    ("f" ibuffer-do-sort-by-filename/process "filename")
    ("m" ibuffer-do-sort-by-major-mode "mode")
    ("p" (ibuffer-projectile-set-filter-groups) "by project enable")
    ("P" (progn
           (spacemacs//ibuffer-create-buffs-group)
           (ibuffer-update nil t))
     "by project disable")
    ("b" hydra-ibuffer-main/body "back" :color blue))
  (defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
    "Filter"
    ("m" ibuffer-filter-by-used-mode "mode")
    ("M" ibuffer-filter-by-derived-mode "derived mode")
    ("n" ibuffer-filter-by-name "name")
    ("c" ibuffer-filter-by-content "content")
    ("e" ibuffer-filter-by-predicate "predicate")
    ("f" ibuffer-filter-by-filename "filename")
    (">" ibuffer-filter-by-size-gt "size")
    ("<" ibuffer-filter-by-size-lt "size")
    ("/" ibuffer-filter-disable "disable")
    ("b" hydra-ibuffer-main/body "back" :color blue))
  (define-key ibuffer-mode-map "." #'hydra-ibuffer-main/body))
