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

(defun yxl/pdf-view-bindings ()
  (interactive)  ; needs to call interactively due to bugs in pdf mode
  (evil-set-initial-state 'pdf-view-mode 'evilified)
  (evilified-state-evilify pdf-view-mode pdf-view-mode-map
    "-" #'dired-jump
    "_" #'pdf-view-shrink
    "+" #'pdf-view-enlarge
    "gg" #'yxl/pdf-view-goto-first-page
    "G" #'yxl/pdf-view-goto-page
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
    "gg" #'yxl/pdf-view-goto-first-page
    "G" #'yxl/pdf-view-goto-page
    "os" #'yxl/helm-pdf-occur
    "oS" #'yxl/pdf-occur-search-preset
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

(defun yxl/pdf-view-goto-page ()
  "vim-style wrapper for pdf-view-goto-page. accepts G or 5G."
  (interactive)
  (if current-prefix-arg
      (pdf-view-goto-page (prefix-numeric-value current-prefix-arg))
    (pdf-view-last-page)))

(defun yxl/pdf-view-goto-first-page ()
  "vim-style wrapper for pdf-view-goto-page. accepts gg or 5gg."
  (interactive)
  (if current-prefix-arg
      (pdf-view-goto-page (prefix-numeric-value current-prefix-arg))
    (pdf-view-first-page)))

(defun yxl/pdf-occur-search-preset ()
  (interactive)
  (pdf-occur yxl-pdf-occur-preset-all t))

(defun zilong/elfeed-mark-all-as-read ()
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))

(defun yxl/elfeed-mark-as-read ()
  (interactive)
  (elfeed-search-untag-all 'unread))
(defun yxl/elfeed-mark-as-unread ()
  (interactive)
  (elfeed-search-tag-all 'unread))

(defun elfeed-toggle-shr-inhibit-images ()
  "toggle the value of shr-inhibit-images"
  (interactive)
  (if (equal shr-inhibit-images t)
      (setq shr-inhibit-images nil)
    (setq shr-inhibit-images t))
  (message "shr-inhibit-images: %s" shr-inhibit-images))

(defun yxl/dired-delete-window ()
  "delete the current dired window, if it is the only window, return to
spacemacs home buffer"
  (interactive)
  (if (one-window-p)
      (spacemacs/home)
    (delete-window)))
