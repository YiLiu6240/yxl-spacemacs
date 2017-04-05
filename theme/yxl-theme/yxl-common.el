(defmacro dyn-let (varlist fn setfaces setvars)
  (list 'let (append varlist (funcall fn)) setfaces setvars))

(defgroup yxl-theme nil
  "Spacemacs-theme options."
  :group 'faces)

(defcustom yxl-theme-set-bg t
  "Use a background for comment lines."
  :type 'boolean
  :group 'yxl-theme)

(defcustom yxl-theme-org-height t
  "Use varying text heights for org headings."
  :type 'boolean
  :group 'yxl-theme)

(defcustom yxl-theme-org-highlight nil
  "Highlight org headings."
  :type 'boolean
  :group 'yxl-theme)

(defcustom yxl-theme-custom-colors nil
  "Specify a list of custom colors"
  :type 'alist
  :group 'yxl-theme)

(defun true-color-p ()
  (or
   (display-graphic-p)
   (= (tty-display-color-cells) 16777216)))

(defun custom-colors-override ()
  (mapcar (lambda (x) (list (car x) (cdr x)))
          yxl-theme-custom-colors))

(defun create-yxl-theme (variant theme-name)
  (dyn-let ((class '((class color) (min-colors 89))) ;;                      ~~ dark ~~                                                  ~~ gruv ~~                                  ~~ light ~~
            ;;                                                              GUI       TER                                               GUI       TER                               GUI       TER
            ;; generic
            ;; mode line active color 1
            (act1          (cond ((eq variant 'dark) (if (true-color-p) "#073642" "#504945")) ((eq variant 'gruv) (if (true-color-p) "#504945" "#504945")) (t (if (true-color-p) "#fcf4dc" "#d7dfff"))))
            ;; mode line active color 2
            (act2          (cond ((eq variant 'dark) (if (true-color-p) "#132b34" "#3c3836")) ((eq variant 'gruv) (if (true-color-p) "#3c3836" "#3c3836")) (t (if (true-color-p) "#e9e2cb" "#afafd7"))))
            ;; basic foreground color
            (base          (cond ((eq variant 'dark) (if (true-color-p) "#839496" "#bdae93")) ((eq variant 'gruv) (if (true-color-p) "#bdae93" "#ffdfaf")) (t (if (true-color-p) "#52676f" "#5f5f87"))))
            ;; lighter version of foreground color
            (base-light    (cond ((eq variant 'dark) (if (true-color-p) "#839496" "#bdae93")) ((eq variant 'gruv) (if (true-color-p) "#fbf1c7" "#fbf1c7")) (t (if (true-color-p) "#52676f" "#5f5f87"))))
            ;; dimmed version of foreground color
            (base-dim      (cond ((eq variant 'dark) (if (true-color-p) "#657b83" "#7c6f64")) ((eq variant 'gruv) (if (true-color-p) "#7c6f64" "#7c6f64")) (t (if (true-color-p) "#81908f" "#afafd7"))))
            ;; normal background color
            ;; comment
            (comment       (cond ((eq variant 'dark) (if (true-color-p) "#586e75" "#7c6f64")) ((eq variant 'gruv) (if (true-color-p) "#7c6f64" "#8a8a8a")) (t (if (true-color-p) "#81908f" "#008787"))))
            ;; comment bg
            (comment-bg    (cond ((eq variant 'dark) (if (true-color-p) "#15262c" "#282828")) ((eq variant 'gruv) (if (true-color-p) "#282828" "#282828")) (t (if (true-color-p) "#fcf4dc" "#ffffff"))))
            ;; bg1: main background
            (bg1           (cond ((eq variant 'dark) (if (true-color-p) "#15262c" "#282828")) ((eq variant 'gruv) (if (true-color-p) "#282828" "#262626")) (t (if (true-color-p) "#fcf4dc" "#ffffff"))))
            ;; current line highlight
            (bg2           (cond ((eq variant 'dark) (if (true-color-p) "#132b34" "#3c3836")) ((eq variant 'gruv) (if (true-color-p) "#3c3836" "#3a3a3a")) (t (if (true-color-p) "#e9e2cb" "#e4e4e4"))))
            (bg2-hl        (cond ((eq variant 'dark) (if (true-color-p) "#1b343d" "#3c3836")) ((eq variant 'gruv) (if (true-color-p) "#3c3836" "#3c3836")) (t (if (true-color-p) "#e9e2cb" "#e4e4e4"))))
            ;; darker shade of bg1
            (bg3           (cond ((eq variant 'dark) (if (true-color-p) "#073642" "#504945")) ((eq variant 'gruv) (if (true-color-p) "#504945" "#504945")) (t (if (true-color-p) "#e9e2cb" "#d0d0d0"))))
            ;; darkeest of bg
            (bg4           (cond ((eq variant 'dark) (if (true-color-p) "#073642" "#504945")) ((eq variant 'gruv) (if (true-color-p) "#504945" "#504945")) (t (if (true-color-p) "#e9e2cb" "#bcbcbc"))))
            ;; mode line border color
            (border        (cond ((eq variant 'dark) (if (true-color-p) "#073642" "#504945")) ((eq variant 'gruv) (if (true-color-p) "#504945" "#504945")) (t (if (true-color-p) "#708183" "#b3b9be"))))
            ;; code block fg
            (cblk          (cond ((eq variant 'dark) (if (true-color-p) "#cbc1d5" "#b2b2b2")) ((eq variant 'gruv) (if (true-color-p) "#b2b2b2" "#b2b2b2")) (t (if (true-color-p) "#655370" "#5f5f87"))))
            ;; code block bg
            (cblk-bg       (cond ((eq variant 'dark) (if (true-color-p) "#2f2b33" "#262626")) ((eq variant 'gruv) (if (true-color-p) "#262626" "#262626")) (t (if (true-color-p) "#e8e3f0" "#ffffff"))))
            ;; code block header line
            (cblk-ln       (cond ((eq variant 'dark) (if (true-color-p) "#827591" "#af5faf")) ((eq variant 'gruv) (if (true-color-p) "#af5faf" "#af5faf")) (t (if (true-color-p) "#9380b2" "#af5fdf"))))
            ;; code block header line bg
            (cblk-ln-bg    (cond ((eq variant 'dark) (if (true-color-p) "#373040" "#333333")) ((eq variant 'gruv) (if (true-color-p) "#333333" "#333333")) (t (if (true-color-p) "#ddd8eb" "#dfdfff"))))
            ;; cursor/point ???
            (cursor        (cond ((eq variant 'dark) (if (true-color-p) "#e3dedd" "#d0d0d0")) ((eq variant 'gruv) (if (true-color-p) "#d0d0d0" "#d0d0d0")) (t (if (true-color-p) "#100a14" "#121212"))))
            ;; constant
            (const         (cond ((eq variant 'dark) (if (true-color-p) "#33859e" "#d3869b")) ((eq variant 'gruv) (if (true-color-p) "#87af87" "#87af87")) (t (if (true-color-p) "#33859e" "#8700af"))))
            ;; complementary
            (comp          (cond ((eq variant 'dark) (if (true-color-p) "#b58900" "#8ec07c")) ((eq variant 'gruv) (if (true-color-p) "#8ec07c" "#8ec07c")) (t (if (true-color-p) "#b58900" "#8700af"))))
            ;; errors
            (err           (cond ((eq variant 'dark) (if (true-color-p) "#dc322f" "#cc241d")) ((eq variant 'gruv) (if (true-color-p) "#cc241d" "#cc241d")) (t (if (true-color-p) "#dc322f" "#e0211d"))))
            ;; functions
            (func          (cond ((eq variant 'dark) (if (true-color-p) "#33859e" "#d79921")) ((eq variant 'gruv) (if (true-color-p) "#d79921" "#ffaf00")) (t (if (true-color-p) "#33859e" "#8700af"))))
            ;; org lv1 heading
            (head1         (cond ((eq variant 'dark) (if (true-color-p) "#33859e" "#458588")) ((eq variant 'gruv) (if (true-color-p) "#458588" "#458588")) (t (if (true-color-p) "#33859e" "#33859e"))))
            (head1-bg      (cond ((eq variant 'dark) (if (true-color-p) "#132b34" "#262626")) ((eq variant 'gruv) (if (true-color-p) "#262626" "#262626")) (t (if (true-color-p) "#edf1ed" "#ffffff"))))
            ;; org lv2 heading
            (head2         (cond ((eq variant 'dark) (if (true-color-p) "#2aa889" "#d79921")) ((eq variant 'gruv) (if (true-color-p) "#b57614" "#d79921")) (t (if (true-color-p) "#2aa889" "#2aa198"))))
            (head2-bg      (cond ((eq variant 'dark) (if (true-color-p) "#293235" "#262626")) ((eq variant 'gruv) (if (true-color-p) "#262626" "#262626")) (t (if (true-color-p) "#edf2e9" "#ffffff"))))
            ;; org lv3 heading
            (head3         (cond ((eq variant 'dark) (if (true-color-p) "#b58900" "#98971a")) ((eq variant 'gruv) (if (true-color-p) "#98971a" "#afaf00")) (t (if (true-color-p) "#b58900" "#5faf00"))))
            (head3-bg      (cond ((eq variant 'dark) (if (true-color-p) "#293235" "#262626")) ((eq variant 'gruv) (if (true-color-p) "#262626" "#262626")) (t (if (true-color-p) "#edf2e9" "#ffffff"))))
            ;; org lv4 heading
            (head4         (cond ((eq variant 'dark) (if (true-color-p) "#86dc2f" "#8ec07c")) ((eq variant 'gruv) (if (true-color-p) "#8ec07c" "#8ec07c")) (t (if (true-color-p) "#859900" "#875f00"))))
            (head4-bg      (cond ((eq variant 'dark) (if (true-color-p) "#32322c" "#262626")) ((eq variant 'gruv) (if (true-color-p) "#262626" "#262626")) (t (if (true-color-p) "#f6f1e1" "#ffffff"))))
            ;; highlighted area, NOTE: this should be different from line highlight
            (highlight     (cond ((eq variant 'dark) (if (true-color-p) "#1d4250" "#3c3836")) ((eq variant 'gruv) (if (true-color-p) "#504945" "#3c3836")) (t (if (true-color-p) "#1d4250" "#d7d7ff"))))
            ;; TODO: work on this
            (highlight-dim (cond ((eq variant 'dark) (if (true-color-p) "#1d4250" "#3c3836")) ((eq variant 'gruv) (if (true-color-p) "#3c3836" "#3c3836")) (t (if (true-color-p) "#1d4250" "#d7d7ff"))))
            ;; keyword
            ;; (keyword       (if (eq variant 'dark) (if (true-color-p) "#859900" "#33859e") (if (true-color-p) "#859900" "#33859e")))
            (keyword       (cond ((eq variant 'dark) (if (true-color-p) "#679a01" "#a16946")) ((eq variant 'gruv) (if (true-color-p) "#fe8019" "#d75f5f")) (t (if (true-color-p) "#679a01" "#33859e"))))
            ;; line numbers
            (lnum          (cond ((eq variant 'dark) (if (true-color-p) "#586e75" "#767676")) ((eq variant 'gruv) (if (true-color-p) "#928374" "#767676")) (t (if (true-color-p) "#44505c" "#af87af"))))
            ;; matched, matching parens, brackets, tags
            (mat           (cond ((eq variant 'dark) (if (true-color-p) "#86dc2f" "#98971a")) ((eq variant 'gruv) (if (true-color-p) "#98971a" "#98971a")) (t (if (true-color-p) "#86dc2f" "#af005f"))))
            ;; meta, org's meta line
            (meta          (cond ((eq variant 'dark) (if (true-color-p) "#6c71c4" "#689d6a")) ((eq variant 'gruv) (if (true-color-p) "#689d6a" "#689d6a")) (t (if (true-color-p) "#6c71c4" "#df5f5f"))))
            ;; string
            (str           (cond ((eq variant 'dark) (if (true-color-p) "#2aa198" "#98971a")) ((eq variant 'gruv) (if (true-color-p) "#98971a" "#afaf00")) (t (if (true-color-p) "#2aa198" "#2aa198"))))
            ;; success
            (suc           (cond ((eq variant 'dark) (if (true-color-p) "#86dc2f" "#98971a")) ((eq variant 'gruv) (if (true-color-p) "#98971a" "#afaf00")) (t (if (true-color-p) "#86dc2f" "#00af00"))))
            ;; tooltip
            (ttip          (cond ((eq variant 'dark) (if (true-color-p) "#586e75" "#504945")) ((eq variant 'gruv) (if (true-color-p) "#bdae93" "#ffdfaf")) (t (if (true-color-p) "#8c799f" "#5f5f87"))))
            ;; tooltip selection
            (ttip-sl       (cond ((eq variant 'dark) (if (true-color-p) "#1b343d" "#3c3836")) ((eq variant 'gruv) (if (true-color-p) "#076678" "#076678")) (t (if (true-color-p) "#c8c6dd" "#afafff"))))
            ;; tooltip bg
            (ttip-bg       (cond ((eq variant 'dark) (if (true-color-p) "#15262c" "#282828")) ((eq variant 'gruv) (if (true-color-p) "#1d2021" "#1d2021")) (t (if (true-color-p) "#e2e0ea" "#dfdfff"))))
            ;; type
            (type          (cond ((eq variant 'dark) (if (true-color-p) "#b58900" "#b16286")) ((eq variant 'gruv) (if (true-color-p) "#d3869b" "#d787af")) (t (if (true-color-p) "#b58900" "#af005f"))))
            ;; variable
            (var           (cond ((eq variant 'dark) (if (true-color-p) "#33859e" "#458588")) ((eq variant 'gruv) (if (true-color-p) "#83a598" "#87afaf")) (t (if (true-color-p) "#33859e" "#af5fd7"))))
            ;; warning
            (war           (cond ((eq variant 'dark) (if (true-color-p) "#dc752f" "#cc241d")) ((eq variant 'gruv) (if (true-color-p) "#cc241d" "#d75f5f")) (t (if (true-color-p) "#dc752f" "#dc752f"))))

            ;; colors
            (aqua          (cond ((eq variant 'dark) (if (true-color-p) "#2d9574" "#8ec07c")) ((eq variant 'gruv) (if (true-color-p) "#8ec07c" "#87af87")) (t (if (true-color-p) "#2d9574" "#2aa198"))))
            (aqua-bg       (cond ((eq variant 'dark) (if (true-color-p) "#293235" "#262626")) ((eq variant 'gruv) (if (true-color-p) "#427b58" "#427b58")) (t (if (true-color-p) "#edf2e9" "#ffffff"))))
            (green         (cond ((eq variant 'dark) (if (true-color-p) "#859900" "#98971a")) ((eq variant 'gruv) (if (true-color-p) "#98971a" "#afaf00")) (t (if (true-color-p) "#859900" "#5faf00"))))
            (green-bg      (cond ((eq variant 'dark) (if (true-color-p) "#293235" "#262626")) ((eq variant 'gruv) (if (true-color-p) "#79740e" "#79740e")) (t (if (true-color-p) "#edf2e9" "#ffffff"))))
            (green-bg-s    (cond ((eq variant 'dark) (if (true-color-p) "#29422d" "#262626")) ((eq variant 'gruv) (if (true-color-p) "#262626" "#262626")) (t (if (true-color-p) "#dae6d0" "#ffffff"))))
            (cyan          (cond ((eq variant 'dark) (if (true-color-p) "#2aa889" "#689d6a")) ((eq variant 'gruv) (if (true-color-p) "#689d6a" "#689d6a")) (t (if (true-color-p) "#2aa889" "#008080"))))
            (orange        (cond ((eq variant 'dark) (if (true-color-p) "#cb4b16" "#d65d0e")) ((eq variant 'gruv) (if (true-color-p) "#d65d0e" "#ff8700")) (t (if (true-color-p) "#cb4b16" "#cb4b16"))))
            (red           (cond ((eq variant 'dark) (if (true-color-p) "#dc322f" "#cc241d")) ((eq variant 'gruv) (if (true-color-p) "#cc241d" "#d75f5f")) (t (if (true-color-p) "#dc322f" "#d70008"))))
            (red-bg        (cond ((eq variant 'dark) (if (true-color-p) "#3c2a2c" "#262626")) ((eq variant 'gruv) (if (true-color-p) "#9d0006" "#9d0006")) (t (if (true-color-p) "#faede4" "#ffffff"))))
            (red-bg-s      (cond ((eq variant 'dark) (if (true-color-p) "#512e31" "#262626")) ((eq variant 'gruv) (if (true-color-p) "#262626" "#262626")) (t (if (true-color-p) "#eed9d2" "#ffffff"))))
            (blue          (cond ((eq variant 'dark) (if (true-color-p) "#4f97d7" "#458588")) ((eq variant 'gruv) (if (true-color-p) "#458588" "#87afaf")) (t (if (true-color-p) "#4f97d7" "#33859e"))))
            (blue-bg       (cond ((eq variant 'dark) (if (true-color-p) "#293239" "#076678")) ((eq variant 'gruv) (if (true-color-p) "#076678" "#076678")) (t (if (true-color-p) "#edf1ed" "#d7d7ff"))))
            ;; TODO: add violet
            (magenta       (cond ((eq variant 'dark) (if (true-color-p) "#d33682" "#b16286")) ((eq variant 'gruv) (if (true-color-p) "#b16286" "#d787af")) (t (if (true-color-p) "#a31db1" "#800080"))))
            (yellow        (cond ((eq variant 'dark) (if (true-color-p) "#b58900" "#d79921")) ((eq variant 'gruv) (if (true-color-p) "#d79921" "#ff8700")) (t (if (true-color-p) "#b58900" "#875f00"))))
            (yellow-bg     (cond ((eq variant 'dark) (if (true-color-p) "#32322c" "#262626")) ((eq variant 'gruv) (if (true-color-p) "#b57614" "#b57614")) (t (if (true-color-p) "#f6f1e1" "#ffffff"))))
            )

           custom-colors-override

           (custom-theme-set-faces
            theme-name

;;;;; basics
            `(cursor ((,class (:background ,cursor))))
            `(custom-button ((,class :background ,bg2 :foreground ,base :box (:line-width 2 :style released-button))))
            `(default ((,class (:background ,bg1 :foreground ,base))))
            `(default-italic ((,class (:italic t))))
            `(error ((,class (:foreground ,err))))
            `(eval-sexp-fu-flash ((,class (:background ,suc :foreground ,bg1))))
            `(eval-sexp-fu-flash-error ((,class (:background ,err :foreground ,bg1))))
            `(font-lock-builtin-face ((,class (:foreground ,const))))
            `(font-lock-comment-face ((,class (:foreground ,comment))))
            `(font-lock-constant-face ((,class (:foreground ,const))))
            `(font-lock-doc-face ((,class (:foreground ,comment))))
            `(font-lock-function-name-face ((,class (:foreground ,func :inherit bold))))
            `(font-lock-keyword-face ((,class (:foreground ,keyword))))
            `(font-lock-negation-char-face ((,class (:foreground ,const))))
            `(font-lock-preprocessor-face ((,class (:foreground ,func))))
            `(font-lock-reference-face ((,class (:foreground ,const))))
            `(font-lock-string-face ((,class (:foreground ,str))))
            `(font-lock-type-face ((,class (:foreground ,type :inherit bold))))
            `(font-lock-variable-name-face ((,class (:foreground ,var))))
            `(font-lock-warning-face ((,class (:foreground ,war))))
            `(fringe ((,class (:foreground ,comment))))
            `(header-line ((,class :background ,bg4)))
            `(highlight ((,class (:foreground ,base :background ,highlight))))
            `(hl-line ((,class (:background ,bg2-hl))))
            `(isearch ((,class (:foreground ,bg1 :background ,mat))))
            `(lazy-highlight ((,class (:background ,blue-bg :weight normal))))
            ;; `(link ((,class (:foreground ,comment :underline t))))
            ;; `(link-visited ((,class (:foreground ,comp :underline t))))
            `(link ((,class (:foreground ,comment))))
            `(link-visited ((,class (:foreground ,comp))))
            `(match ((,class (:background ,highlight :foreground ,mat))))
            `(minibuffer-prompt ((,class (:inherit bold :foreground ,keyword))))
            `(page-break-lines ((,class (:foreground ,act2))))
            `(region ((,class (:background ,highlight))))
            `(secondary-selection ((,class (:background ,bg3))))
            `(success ((,class (:foreground ,suc))))
            `(tooltip ((,class (:background ,ttip-sl :foreground ,base :bold nil :italic nil :underline nil))))
            `(vertical-border ((,class (:foreground ,bg4))))
            `(warning ((,class (:foreground ,war))))
            `(bold ((,class (:foreground "#0f7095" :weight bold))))
            `(bold-italic ((,class (:foreground "#2aa198" :slant italic :weight bold))))
            `(italic ((,class (:foreground "#2aa198" :slant italic))))

;;;;; ahs
            `(ahs-face ((,class (:background ,highlight))))
            `(ahs-plugin-whole-buffer-face ((,class (:background ,mat :foreground ,bg1))))

;;;;; anzu-mode
            `(anzu-mode-line ((,class (:foreground ,yellow :inherit bold))))

;;;;; auto-complete
            `(ac-completion-face ((,class (:background ,ttip-bg :foreground ,ttip))))

;;;;; avy
            `(avy-lead-face   ((,class (:background ,bg2 :foreground ,magenta))))
            `(avy-lead-face-0 ((,class (:background ,bg2 :foreground ,blue))))
            `(avy-lead-face-1 ((,class (:background ,bg2 :foreground ,magenta))))
            `(avy-lead-face-2 ((,class (:background ,bg2 :foreground ,blue))))

;;;;; bm
            `(bm-face ((,class (:background "#245361" :foreground ,base))))
            `(bm-fringe-face ((,class (:background "#245361" :foreground ,base))))
            `(bm-persistent-face ((,class (:background "#d26937" :foreground "#0732642"))))
            `(bm-fringe-persistent-face ((,class (:background "#d26937" :foreground "#073642"))))

;;;;; cider
            `(cider-enlightened ((,class (:background nil :box (:color ,yellow :line-width -1 :style nil) :foreground ,yellow))))
            `(cider-enlightened-local ((,class (:foreground ,yellow))))
            `(cider-instrumented-face ((,class (:background nil :box (:color ,red :line-width -1 :style nil) :foreground ,red))))
            `(cider-result-overlay-face ((,class (:background nil :box (:color ,blue :line-width -1 :style nil) :foreground ,blue))))
            `(cider-test-error-face ((,class (:background ,war :foreground ,bg1))))
            `(cider-test-failure-face ((,class (:background ,err :foreground ,bg1))))
            `(cider-test-success-face ((,class (:background ,suc :foreground ,bg1))))
            `(cider-traced-face ((,class :box (:color ,cyan :line-width -1 :style nil))))

;;;;; company
            `(company-echo-common ((,class (:background ,base :foreground ,bg1))))
            `(company-preview ((,class (:background ,ttip-bg :foreground ,ttip))))
            `(company-preview-common ((,class (:background ,ttip-bg :foreground ,base))))
            `(company-preview-search ((,class (:inherit match))))
            `(company-scrollbar-bg ((,class (:background ,bg2))))
            `(company-scrollbar-fg ((,class (:background ,act2))))
            `(company-template-field ((,class (:inherit region))))
            `(company-tooltip ((,class (:background ,ttip-bg :foreground ,ttip))))
            `(company-tooltip-annotation ((,class (:foreground ,keyword))))
            `(company-tooltip-selection ((,class (:background ,ttip-sl :foreground ,base))))
            `(company-tooltip-common ((,class (:inherit company-tooltip :weight bold :underline nil))))
            `(company-tooltip-common-selection ((,class (:inherit company-tooltip-selection :weight bold :underline nil))))
            `(company-tooltip-mouse ((,class (:inherit highlight))))
            `(company-tooltip-search ((,class (:inherit match))))

;;;;; calfw
            `(cfw:face-title ((t (:foreground ,head1 :weight bold :height 2.0))))
            `(cfw:face-header ((t (:foreground ,head2 :weight bold))))
            `(cfw:face-sunday ((t :foreground ,head3 :background ,bg2 :weight bold)))
            `(cfw:face-saturday ((t :foreground ,head3 :background ,bg2 :weight bold)))
            `(cfw:face-holiday ((t :foreground ,head4 :background ,bg2 :weight bold)))
            `(cfw:face-grid ((t :foreground ,bg2)))
            `(cfw:face-default-content ((t :foreground ,base)))
            `(cfw:face-periods ((t :foreground ,keyword)))
            `(cfw:face-day-title ((t :background ,bg2)))
            `(cfw:face-today-title ((t :foreground ,suc :background ,bg3 :weight bold)))
            `(cfw:face-default-day ((t :weight bold :inherit cfw:face-day-title)))
            `(cfw:face-annotation ((t :foreground "RosyBrown" :inherit cfw:face-day-title)))
            `(cfw:face-disable ((t :foreground "DarkGray" :inherit cfw:face-day-title)))
            `(cfw:face-today ((t :background: ,bg2 :weight bold)))
            `(cfw:face-select ((t :background ,bg2)))
            `(cfw:face-toolbar ((t :foreground ,bg1 :background ,bg1)))
            `(cfw:face-toolbar-button-off ((t :foreground ,comment :weight bold)))
            `(cfw:face-toolbar-button-on ((t :foreground ,base :weight bold)))

;;;;; diff
            `(diff-added             ((,class :background nil :foreground ,green)))
            `(diff-changed           ((,class :background nil :foreground ,keyword)))
            `(diff-header            ((,class :background ,cblk-ln-bg :foreground ,func)))
            `(diff-indicator-added   ((,class :background nil :foreground ,green)))
            `(diff-indicator-changed ((,class :background nil :foreground ,keyword)))
            `(diff-indicator-removed ((,class :background nil :foreground ,red)))
            `(diff-refine-added      ((,class :background ,green :foreground ,bg4)))
            `(diff-refine-changed    ((,class :background ,keyword :foreground ,bg4)))
            `(diff-refine-removed    ((,class :background ,red :foreground ,bg4)))
            `(diff-removed           ((,class :background nil :foreground ,red)))

;;;;; diff-hl
            `(diff-hl-change ((,class :background ,blue-bg :foreground ,blue)))
            `(diff-hl-delete ((,class :background ,red-bg :foreground ,red)))
            `(diff-hl-insert ((,class :background ,green-bg :foreground ,green)))

;;;;; dired
            `(dired-directory ((,class (:foreground ,func :weight normal))))
            `(dired-flagged ((,class (:foreground ,red))))
            `(dired-header ((,class (:foreground ,comp :inherit bold))))
            `(dired-ignored ((,class (:inherit shadow))))
            `(dired-mark ((,class (:foreground ,comp :inherit bold))))
            `(dired-marked ((,class (:foreground ,magenta :inherit bold))))
            `(dired-perm-write ((,class (:foreground ,base :underline t))))
            `(dired-symlink ((,class (:foreground ,cyan :background ,bg1 :inherit bold))))
            `(dired-warning ((,class (:foreground ,war))))

;;;;; ediff
            `(ediff-current-diff-A ((,class(:background ,red-bg-s :foreground ,red))))
            `(ediff-current-diff-Ancestor ((,class(:background ,aqua-bg :foreground ,aqua))))
            `(ediff-current-diff-B ((,class(:background ,green-bg-s :foreground ,green))))
            `(ediff-current-diff-C ((,class(:background ,blue-bg :foreground ,blue))))
            `(ediff-even-diff-A ((,class(:background ,bg3))))
            `(ediff-even-diff-Ancestor ((,class(:background ,bg3))))
            `(ediff-even-diff-B ((,class(:background ,bg3))))
            `(ediff-even-diff-C ((,class(:background ,bg3))))
            `(ediff-fine-diff-A ((,class(:background nil :inherit bold))))
            `(ediff-fine-diff-Ancestor ((,class(:background nil :inherit bold))))
            `(ediff-fine-diff-B ((,class(:background nil :inherit bold))))
            `(ediff-fine-diff-C ((,class(:background nil :inherit bold))))
            `(ediff-odd-diff-A ((,class(:background ,bg4))))
            `(ediff-odd-diff-Ancestor ((,class(:background ,bg4))))
            `(ediff-odd-diff-B ((,class(:background ,bg4))))
            `(ediff-odd-diff-C ((,class(:background ,bg4))))

;;;;; ein
            `(ein:cell-input-area((,class (:background ,bg2))))
            `(ein:cell-input-prompt ((,class (:foreground ,suc))))
            `(ein:cell-output-prompt ((,class (:foreground ,err))))
            `(ein:notification-tab-normal ((,class (:foreground ,keyword))))
            `(ein:notification-tab-selected ((,class (:foreground ,suc :inherit bold))))

;;;;; eldoc
            `(eldoc-highlight-function-argument ((,class (:foreground ,mat :inherit bold))))

;;;;; elfeed
            `(elfeed-search-title-face ((t (:foreground ,comment))))
            `(elfeed-search-unread-title-face ((t (:foreground ,base))))
            `(elfeed-search-feed-face ((t (:foreground ,type))))
            `(elfeed-search-tag-face ((t (:foreground ,keyword))))
            `(elfeed-search-date-face ((,class (:foreground ,head2))))

;;;;; enh-ruby
            `(enh-ruby-string-delimiter-face ((,class (:foreground ,str))))
            `(enh-ruby-op-face ((,class (:background ,bg1 :foreground ,base))))

;;;;; erc
            `(erc-input-face ((,class (:foreground ,func))))
            `(erc-my-nick-face ((,class (:foreground ,keyword))))
            `(erc-nick-default-face ((,class (:foreground ,keyword))))
            `(erc-nick-prefix-face ((,class (:foreground ,yellow))))
            `(erc-notice-face ((,class (:foreground ,str))))
            `(erc-prompt-face ((,class (:foreground ,mat :inherit bold))))
            `(erc-timestamp-face ((,class (:foreground ,keyword))))
;;;;; ess
            `(ess-function-call-face ((,class (:inherit font-lock-function-name-face :weight normal))))

;;;;; eshell
            `(eshell-ls-archive ((,class (:foreground ,red :inherit bold))))
            `(eshell-ls-backup ((,class (:inherit font-lock-comment-face))))
            `(eshell-ls-clutter ((,class (:inherit font-lock-comment-face))))
            `(eshell-ls-directory ((,class (:foreground ,keyword :inherit bold))))
            `(eshell-ls-executable ((,class (:foreground ,suc :inherit bold))))
            `(eshell-ls-missing ((,class (:inherit font-lock-warning-face))))
            `(eshell-ls-product ((,class (:inherit font-lock-doc-face))))
            `(eshell-ls-special ((,class (:foreground ,yellow :inherit bold))))
            `(eshell-ls-symlink ((,class (:foreground ,cyan :inherit bold))))
            `(eshell-ls-unreadable ((,class (:foreground ,base))))
            `(eshell-prompt ((,class (:foreground ,keyword :inherit bold))))

;;;;; evil
            `(evil-ex-substitute-matches ((,class (:background ,red-bg :foreground ,red))))
            `(evil-ex-substitute-replacement ((,class (:background ,green-bg :foreground ,green))))
            `(evil-search-highlight-persist-highlight-face ((,class (:background ,green-bg :foreground ,base-light))))

;;;;; evil-snipe
            `(evil-snipe-first-match-face ((,class (:inherit isearch :foreground ,base-light))))
            `(evil-snipe--match-face ((,class (:inherit region))))

;;;;; eyebrowse
            `(eyebrowse-mode-line-active ((,class (:foreground ,mat :weight bold))))
            `(eyebrowse-mode-line-inactive ((,class (:foreground ,base))))
            `(eyebrowse-mode-line-separator ((,class (:foreground ,base))))
            `(eyebrowse-mode-line-delimiters ((,class (:foreground ,base))))

;;;;; flycheck
            `(flycheck-error
              ((,(append '((supports :underline (:style line))) class)
                (:underline (:style line :color ,err)))
               (,class (:foreground ,base :background ,err :inherit bold :underline t))))
            `(flycheck-error-list-checker-name ((,class (:foreground ,keyword))))
            `(flycheck-fringe-error ((,class (:foreground ,err :inherit bold))))
            `(flycheck-fringe-info ((,class (:foreground ,keyword :inherit bold))))
            `(flycheck-fringe-warning ((,class (:foreground ,war :inherit bold))))
            `(flycheck-info
              ((,(append '((supports :underline (:style line))) class)
                (:underline (:style line :color ,keyword)))
               (,class (:foreground ,base :background ,keyword :inherit bold :underline t))))
            `(flycheck-warning
              ((,(append '((supports :underline (:style line))) class)
                (:underline (:style line :color ,war)))
               (,class (:foreground ,base :background ,war :inherit bold :underline t))))

;;;;; jabber
            `(jabber-activity-face ((,class (:inherit bold :foreground ,red))))
            `(jabber-activity-personal-face ((,class (:inherit bold :foreground ,blue))))
            `(jabber-chat-error ((,class (:inherit bold :foreground ,red))))
            `(jabber-chat-prompt-foreign ((,class (:inherit bold :foreground ,red))))
            `(jabber-chat-prompt-local ((,class (:inherit bold :foreground ,blue))))
            `(jabber-chat-prompt-system ((,class (:inherit bold :foreground ,green))))
            `(jabber-chat-text-foreign ((,class (:foreground ,base))))
            `(jabber-chat-text-local ((,class (:foreground ,base))))
            `(jabber-rare-time-face ((,class (:foreground ,green))))
            `(jabber-roster-user-away ((,class (:foreground ,yellow))))
            `(jabber-roster-user-chatty ((,class (:inherit bold :foreground ,green))))
            `(jabber-roster-user-dnd ((,class (:foreground ,red))))
            `(jabber-roster-user-error ((,class (:foreground ,err))))
            `(jabber-roster-user-offline ((,class (:foreground ,base))))
            `(jabber-roster-user-online ((,class (:inherit bold :foreground ,green))))
            `(jabber-roster-user-xa ((,class (:foreground ,aqua))))

;;;;; git-gutter-fr
            `(git-gutter-fr:added ((,class (:foreground ,green :inherit bold))))
            `(git-gutter-fr:deleted ((,class (:foreground ,war :inherit bold))))
            `(git-gutter-fr:modified ((,class (:foreground ,keyword :inherit bold))))

;;;;; git-timemachine
            `(git-timemachine-minibuffer-detail-face ((,class (:foreground ,blue :inherit bold :background ,blue-bg))))

;;;;; gnus
            `(gnus-emphasis-highlight-words ((,class (:background ,suc :foreground ,bg1))))
            `(gnus-header-content ((,class (:foreground ,keyword))))
            `(gnus-header-from ((,class (:foreground ,var))))
            `(gnus-header-name ((,class (:foreground ,comp))))
            `(gnus-header-subject ((,class (:foreground ,func :inherit bold))))
            `(gnus-summary-cancelled ((,class (:background ,war :foreground ,bg1))))

;;;;; guide-key
            `(guide-key/highlight-command-face ((,class (:foreground ,base))))
            `(guide-key/key-face ((,class (:foreground ,keyword))))
            `(guide-key/prefix-command-face ((,class (:foreground ,keyword :inherit bold))))

;;;;; helm
            `(helm-bookmark-directory ((,class (:inherit helm-ff-directory))))
            `(helm-bookmark-file ((,class (:foreground ,base))))
            `(helm-bookmark-gnus ((,class (:foreground ,comp))))
            `(helm-bookmark-info ((,class (:foreground ,comp))))
            `(helm-bookmark-man ((,class (:foreground ,comp))))
            `(helm-bookmark-w3m ((,class (:foreground ,comp))))
            `(helm-buffer-directory ((,class (:foreground ,base :background ,bg1))))
            `(helm-buffer-file ((,class (:foreground ,base :background ,bg1))))
            `(helm-buffer-not-saved ((,class (:foreground ,comp :background ,bg1))))
            `(helm-buffer-process ((,class (:foreground ,keyword :background ,bg1))))
            `(helm-buffer-saved-out ((,class (:foreground ,base :background ,bg1))))
            `(helm-buffer-size ((,class (:foreground ,base :background ,bg1))))
            `(helm-candidate-number ((,class (:background ,bg1 :foreground ,keyword :inherit bold))))
            `(helm-ff-directory ((,class (:foreground ,keyword :background ,bg1 :inherit bold))))
            `(helm-ff-dotted-directory ((,class (:foreground ,keyword :background ,bg1 :inherit bold))))
            `(helm-ff-dotted-symlink-directory ((,class (:foreground ,cyan :background ,bg1 :inherit bold))))
            `(helm-ff-executable ((,class (:foreground ,suc :background ,bg1 :weight normal))))
            `(helm-ff-file ((,class (:foreground ,base :background ,bg1 :weight normal))))
            `(helm-ff-invalid-symlink ((,class (:foreground ,red :background ,bg1 :inherit bold))))
            `(helm-ff-prefix ((,class (:foreground ,bg1 :background ,keyword :weight normal))))
            `(helm-ff-symlink ((,class (:foreground ,cyan :background ,bg1 :inherit bold))))
            `(helm-grep-cmd-line ((,class (:foreground ,base :background ,bg1))))
            `(helm-grep-file ((,class (:foreground ,base :background ,bg1))))
            `(helm-grep-finish ((,class (:foreground ,base :background ,bg1))))
            `(helm-grep-lineno ((,class (:foreground ,type :background ,bg1 :inherit bold))))
            `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
            `(helm-header ((,class (:foreground ,base :background ,bg1 :underline nil :box nil))))
            `(helm-header-line-left-margin ((,class (:foreground ,keyword :background ,nil))))
            `(helm-match ((,class (:background ,head1-bg :foreground ,head1))))
            `(helm-match-item ((,class (:background ,head1-bg :foreground ,head1))))
            `(helm-moccur-buffer ((,class (:foreground ,var :background ,bg1))))
            `(helm-selection ((,class (:background ,ttip-sl))))
            `(helm-selection-line ((,class (:background ,bg2))))
            `(helm-separator ((,class (:foreground ,comp :background ,bg1))))
            `(helm-source-header ((,class (:background ,comp :foreground ,bg1 :inherit bold))))
            `(helm-time-zone-current ((,class (:foreground ,keyword :background ,bg1))))
            `(helm-time-zone-home ((,class (:foreground ,comp :background ,bg1))))
            `(helm-visible-mark ((,class (:foreground ,keyword :background ,bg3))))

;;;;; helm-swoop
            `(helm-swoop-target-line-block-face ((,class (:foreground ,base :background ,highlight))))
            `(helm-swoop-target-line-face ((,class (:background ,highlight))))
            `(helm-swoop-target-word-face ((,class (:background ,highlight :foreground ,mat))))

;;;;; highlights
            `(hi-yellow ((,class (:foreground ,base-light :background ,yellow-bg))))
            `(hi-green  ((,class (:foreground ,base-light :background ,green-bg))))
            `(hi-blue  ((,class (:foreground ,base-light :background ,blue-bg))))
            `(hi-pink  ((,class (:foreground ,base-light :background ,aqua-bg))))

;;;;; highlight-indentation
            `(highlight-indentation-face ((,class (:background ,comment-bg))))
            `(highlight-indentation-current-column-face ((,class (:background ,bg2))))

;;;;; highlight-numbers
            `(highlight-numbers-number ((,class (:inherit font-lock-type-face))))

;;;;; highlight-symbol
            `(highlight-symbol-face ((,class (:background ,bg2))))

;;;;; hydra
            `(hydra-face-blue ((,class (:foreground ,blue))))
            `(hydra-face-red ((,class (:foreground ,red))))

;;;;; ido
            `(ido-first-match ((,class (:foreground ,comp :inherit bold))))
            `(ido-only-match ((,class (:foreground ,mat :inherit bold))))
            `(ido-subdir ((,class (:foreground ,keyword))))
            `(ido-vertical-match-face ((,class (:foreground ,comp :underline nil))))

;;;;; iedit
            `(iedit-occurrence ((,class (:inherit highlight :background ,mat))))

;;;;; info
            `(info-header-xref ((,class (:foreground ,func :underline t))))
            `(info-menu ((,class (:foreground ,suc))))
            `(info-node ((,class (:foreground ,func :inherit bold))))
            `(info-quoted-name ((,class (:foreground ,keyword))))
            `(info-reference-item ((,class (:background nil :underline t :inherit bold))))
            `(info-string ((,class (:foreground ,str))))
            `(info-title-1 ((,class (:height 1.4 :inherit bold))))
            `(info-title-2 ((,class (:height 1.3 :inherit bold))))
            `(info-title-3 ((,class (:height 1.3))))
            `(info-title-4 ((,class (:height 1.2))))

;;;;; ivy
            `(ivy-current-match ((,class (:background ,ttip-sl :foreground ,base))))
            `(ivy-minibuffer-match-face-1 ((,class (:inherit bold))))
            `(ivy-minibuffer-match-face-2 ((,class (:foreground ,head1 :underline t))))
            `(ivy-minibuffer-match-face-3 ((,class (:foreground ,head4 :underline t))))
            `(ivy-minibuffer-match-face-4 ((,class (:foreground ,head3 :underline t))))
            `(ivy-remote ((,class (:foreground ,cyan))))
            `(ivy-virtual ((,class (:foreground ,comment))))

;;;;; langtool
            `(langtool-errline ((,class (:foreground ,base-light :background ,red-bg))))
            `(langtool-correction-face ((,class (:foreground ,base-light :background ,yellow-bg))))

;;;;; latex
            `(font-latex-math-face ((,class (:foreground ,type))))
            `(font-latex-verbatim-face ((,class (:inherit font-lock-string-face))))
            `(font-latex-bold-face ((,class (:foreground ,comp))))
            `(font-latex-italic-face ((,class (:foreground ,keyword :italic t))))
            `(font-latex-match-reference-keywords ((,class (:foreground ,const))))
            `(font-latex-match-variable-keywords ((,class (:foreground ,var))))
            ;; part
            `(font-latex-sectioning-0-face ((,class (:inherit font-latex-sectioning-1-face))))
            ;; chapter
            `(font-latex-sectioning-1-face ((,class (:inherit font-latex-sectioning-1-face))))
            ;; section
            `(font-latex-sectioning-2-face ((,class (:foreground ,head1, :weight bold))))
            ;; subsection
            `(font-latex-sectioning-3-face ((,class (:foreground ,head2 :weight bold))))
            ;; subsubsection
            `(font-latex-sectioning-4-face ((,class (:foreground ,head3 :weight bold))))
            ;; paragraph
            `(font-latex-sectioning-5-face ((,class (:foreground ,head4 :weight bold))))
            `(font-latex-sectioning-6-face ((,class (:foreground ,head4))))
            `(font-latex-string-face ((,class (:foreground ,str))))
            `(font-latex-sedate-face ((,class (:foreground ,comment))))
            `(font-latex-warning-face ((,class (:foreground ,str))))
            `(font-latex-script-char-face ((,class (:foreground ,str))))

;;;;; linum-mode
            `(linum ((,class (:foreground ,lnum :background ,bg1))))

;;;;; linum-relative
            `(linum-relative-current-face ((,class (:foreground ,comp))))

;;;;; magit
            `(magit-blame-culprit ((,class :background ,yellow-bg :foreground ,yellow)))
            `(magit-blame-header  ((,class :background ,yellow-bg :foreground ,green)))
            `(magit-blame-sha1    ((,class :background ,yellow-bg :foreground ,func)))
            `(magit-blame-subject ((,class :background ,yellow-bg :foreground ,yellow)))
            `(magit-blame-time    ((,class :background ,yellow-bg :foreground ,green)))
            `(magit-blame-name    ((,class :background ,yellow-bg :foreground ,yellow)))
            `(magit-blame-heading ((,class :background ,yellow-bg :foreground ,green)))
            `(magit-blame-hash    ((,class :background ,yellow-bg :foreground ,func)))
            `(magit-blame-summary ((,class :background ,yellow-bg :foreground ,yellow)))
            `(magit-blame-date    ((,class :background ,yellow-bg :foreground ,green)))
            `(magit-branch ((,class (:foreground ,const :inherit bold))))
            `(magit-branch-current ((,class (:background ,blue-bg :foreground ,blue :inherit bold :box t))))
            `(magit-branch-local ((,class (:background ,blue-bg :foreground ,blue :inherit bold))))
            `(magit-branch-remote ((,class (:background ,aqua-bg :foreground ,aqua :inherit bold))))
            `(magit-diff-context-highlight ((,class (:background ,bg2 :foreground ,base))))
            `(magit-diff-file-header ((,class (:background ,comment-bg :foreground ,orange))))
            `(magit-diff-file-heading ((,class (:background ,comment-bg :foreground ,orange))))
            `(magit-diff-file-heading-highlight ((,class (:inherit magit-diff-file-heading))))
            `(magit-diff-hunk-header ((,class (:background ,ttip-bg :foreground ,ttip))))
            `(magit-diff-hunk-heading ((,class (:background ,ttip-bg :foreground ,ttip))))
            `(magit-diff-hunk-heading-highlight ((,class (:background ,ttip-bg :foreground ,ttip))))
            `(magit-hash ((,class (:foreground ,var))))
            `(magit-hunk-heading           ((,class (:background ,bg3))))
            `(magit-hunk-heading-highlight ((,class (:background ,bg3))))
            `(magit-item-highlight ((,class :background ,bg2)))
            `(magit-log-author ((,class (:foreground ,func))))
            `(magit-log-head-label-head ((,class (:background ,yellow :foreground ,bg1 :inherit bold))))
            `(magit-log-head-label-local ((,class (:background ,keyword :foreground ,bg1 :inherit bold))))
            `(magit-log-head-label-remote ((,class (:background ,suc :foreground ,bg1 :inherit bold))))
            `(magit-log-head-label-tags ((,class (:background ,magenta :foreground ,bg1 :inherit bold))))
            `(magit-log-head-label-wip ((,class (:background ,cyan :foreground ,bg1 :inherit bold))))
            `(magit-log-sha1 ((,class (:foreground ,str))))
            `(magit-process-ng ((,class (:foreground ,war :inherit bold))))
            `(magit-process-ok ((,class (:foreground ,func :inherit bold))))
            `(magit-section-heading        ((,class (:foreground ,keyword :inherit bold))))
            `(magit-section-highlight      ((,class (:background ,bg2))))
            `(magit-section-title ((,class (:background ,bg1 :foreground ,keyword :inherit bold))))
            `(magit-tag ((,class (:foreground ,type :inherit bold :box t))))

;;;;; man
            `(Man-overstrike ((,class (:foreground ,head1 :inherit bold))))
            `(Man-reverse ((,class (:foreground ,highlight))))
            `(Man-underline ((,class (:foreground ,comp :underline t))))

;;;;; markdown
            `(markdown-header-face-1 ((,class (:inherit bold :foreground ,head1 :height ,(if yxl-theme-org-height 1.3 1.0) :background ,(when yxl-theme-org-highlight head1-bg)))))
            `(markdown-header-face-2 ((,class (:inherit bold :foreground ,head2 :height ,(if yxl-theme-org-height 1.2 1.0) :background ,(when yxl-theme-org-highlight head2-bg)))))
            `(markdown-header-face-3 ((,class (:bold nil :foreground ,head3 :height ,(if yxl-theme-org-height 1.1 1.0) :background ,(when yxl-theme-org-highlight head3-bg)))))
            `(markdown-header-face-4 ((,class (:bold nil :foreground ,head4 :background ,(when yxl-theme-org-highlight head4-bg)))))
            `(markdown-header-face-5 ((,class (:bold nil :foreground ,head1))))
            `(markdown-header-face-6 ((,class (:bold nil :foreground ,head2))))
            `(markdown-link-face ((,class (:inherit link))))

;;;;; mode-line
            `(mode-line           ((,class (:foreground ,base :background ,act1 :box (:color ,border :line-width 1)))))
            `(mode-line-inactive  ((,class (:foreground ,base :background ,bg1  :box (:color ,border :line-width 1)))))
            `(mode-line-buffer-id ((,class (:inherit bold :foreground ,func))))

;;;;; mu4e
            `(mu4e-cited-1-face ((,class (:foreground ,base))))
            `(mu4e-cited-7-face ((,class (:foreground ,base))))
            `(mu4e-header-marks-face ((,class (:foreground ,comp))))
            `(mu4e-header-key-face ((,class (:foreground ,head2 :inherit bold))))
            `(mu4e-view-url-number-face ((,class (:foreground ,comp))))
            `(mu4e-unread-face ((,class (:foreground ,yellow :inherit bold))))

;;;;; neotree
            `(neo-dir-link-face ((,class (:foreground ,keyword :inherit bold))))
            `(neo-expand-btn-face ((,class (:foreground ,base))))
            `(neo-file-link-face ((,class (:foreground ,base))))
            `(neo-root-dir-face ((,class (:foreground ,func :inherit bold))))

;;;;; notmuch
            `(notmuch-search-date ((,class (:foreground ,func))))
            `(notmuch-search-flagged-face ((,class (:weight extra-bold))))
            `(notmuch-search-non-matching-authors ((,class (:foreground ,base-dim))))
            `(notmuch-search-unread-face ((,class (:background ,highlight-dim :box ,border))))
            `(notmuch-tag-face ((,class (:foreground ,keyword))))
            `(notmuch-tag-flagged ((,class (:foreground ,war))))

;;;;; org
            `(org-agenda-clocking ((,class (:background ,highlight :foreground ,comp))))
            `(org-agenda-date ((,class (:foreground ,var :height ,(if yxl-theme-org-height 1.1 1.0)))))
            `(org-agenda-date-today ((,class (:foreground ,keyword :slant italic :inherit bold :height ,(if yxl-theme-org-height 1.3 1.0)))))
            `(org-agenda-date-weekend ((,class (:inherit bold :foreground ,var))))
            `(org-agenda-done ((,class (:foreground ,suc :height ,(if yxl-theme-org-height 1.2 1.0)))))
            `(org-agenda-structure ((,class (:inherit bold :foreground ,comp))))
            ;; `(org-block ((,class (:background ,cblk-bg :foreground ,cblk))))
            `(org-block ((,class (:background ,cblk-bg))))
            ;; `(org-block-begin-line ((,class (:background ,cblk-ln-bg :foreground ,cblk-ln))))
            ;; `(org-block-end-line ((,class (:background ,cblk-ln-bg :foreground ,cblk-ln))))
            `(org-block-begin-line ((,class (:foreground ,comment))))
            `(org-block-end-line ((,class (:inherit org-block-begin-line))))
            `(org-clock-overlay ((,class (:foreground ,comp))))
            `(org-code ((,class (:foreground ,cyan))))
            `(org-column ((,class (:background ,highlight))))
            `(org-column-title ((,class (:background ,highlight))))
            `(org-date ((,class (:underline t :foreground ,comment :slant italic :height 0.8))))
            `(org-date-selected ((,class (:background ,func :foreground ,bg1))))
            `(org-document-info-keyword ((,class (:foreground ,meta))))
            `(org-document-title ((,class (:foreground ,func :inherit bold :height ,(if yxl-theme-org-height 1.4 1.0) :underline nil))))
            `(org-done ((,class (:foreground ,comment :inherit bold))))
            `(org-ellipsis ((,class (:foreground ,keyword))))
            `(org-footnote  ((,class (:underline t :foreground ,base))))
            `(org-hide ((,class (:foreground ,bg1))))
            `(org-indent ((,class (:foreground ,bg1))))
            `(org-kbd ((,class (:inherit region :foreground ,base :box (:line-width 1 :style released-button)))))
            `(org-level-1 ((,class (:inherit bold :foreground ,head1 :height ,(if yxl-theme-org-height 1.3 1.0) :background ,(when yxl-theme-org-highlight head1-bg)))))
            `(org-level-2 ((,class (:inherit bold :foreground ,head2 :height ,(if yxl-theme-org-height 1.2 1.0) :background ,(when yxl-theme-org-highlight head2-bg)))))
            `(org-level-3 ((,class (:bold nil :foreground ,head3 :height ,(if yxl-theme-org-height 1.1 1.0) :background ,(when yxl-theme-org-highlight head3-bg)))))
            `(org-level-4 ((,class (:bold nil :foreground ,head4 :background ,(when yxl-theme-org-highlight head4-bg)))))
            `(org-level-5 ((,class (:bold nil :foreground ,head1))))
            `(org-level-6 ((,class (:bold nil :foreground ,head2))))
            `(org-level-7 ((,class (:bold nil :foreground ,head3))))
            `(org-level-8 ((,class (:bold nil :foreground ,head4))))
            `(org-link ((,class (:foreground ,comment))))
            `(org-meta-line ((,class (:foreground ,comment))))
            `(org-mode-line-clock-overrun ((,class (:foreground ,err))))
            `(org-priority ((,class (:foreground ,war :inherit bold))))
            `(org-quote ((,class (:inherit org-block :slant italic))))
            `(org-scheduled ((,class (:foreground ,comp))))
            `(org-scheduled-today ((,class (:foreground ,func :height ,(if yxl-theme-org-height 1.2 1.0)))))
            `(org-sexp-date ((,class (:foreground ,base))))
            `(org-special-keyword ((,class (:foreground ,comment :slant italic :height 0.8))))
            `(org-table ((,class (:foreground ,base :background ,head1-bg))))
            `(org-time-grid ((,class (:foreground ,str))))
            `(org-todo ((,class (:foreground ,green :inherit bold))))
            `(org-verbatim ((,class (:foreground ,keyword))))
            `(org-verse ((,class (:inherit org-block :slant italic))))
            `(org-warning ((,class (:foreground ,err))))
            `(org-tag ((,class (:foreground ,comment))))
            `(org-latex-and-related ((,class (:foreground ,type))))

;;;;; perspective
            `(persp-selected-face ((,class (:inherit bold :foreground ,func))))

;;;;; popup
            `(popup-face ((,class (:background ,ttip-bg :foreground ,ttip))))
            `(popup-tip-face ((,class (:background ,ttip-sl :foreground ,base :bold nil :italic nil :underline nil))))
            `(popup-menu-face ((,class (:background ,ttip-bg :foreground ,base))))
            `(popup-enu-selection-face ((,class (:background ,ttip-sl :foreground ,base))))
            `(popup-menu-mouse-face ((,class (:inherit highlight))))
            `(popup-isearch-match ((,class (:inherit match))))
            `(popup-scroll-bar-foreground-face ((,class (:background ,act2))))
            `(popup-scroll-bar-background-face ((,class (:background ,bg2))))

;;;;; powerline
            ;; `(powerline-active1 ((,class (:background ,act2 :foreground ,base))))
            ;; `(powerline-active2 ((,class (:background ,act2 :foreground ,base))))
            ;; `(powerline-inactive1 ((,class (:background ,bg2 :foreground ,base))))
            ;; `(powerline-inactive2 ((,class (:background ,bg2 :foreground ,base))))
            ;; vim-powerline version
            `(powerline-active1 ((,class (:background "#585858" :foreground "#ffffff"))))
            `(powerline-active2 ((,class (:background "#585858" :foreground "#ffffff"))))
            `(powerline-inactive1 ((,class (:background "#262626" :foreground "#8a8a8a"))))
            `(powerline-inactive2 ((,class (:background "#262626" :foreground "#8a8a8a"))))

;;;;; rainbow-delimiters
            `(rainbow-delimiters-depth-1-face ((,class :foreground ,keyword)))
            `(rainbow-delimiters-depth-2-face ((,class :foreground ,func)))
            `(rainbow-delimiters-depth-3-face ((,class :foreground ,str)))
            `(rainbow-delimiters-depth-4-face ((,class :foreground ,green)))
            `(rainbow-delimiters-depth-5-face ((,class :foreground ,yellow)))
            `(rainbow-delimiters-depth-6-face ((,class :foreground ,keyword)))
            `(rainbow-delimiters-depth-7-face ((,class :foreground ,func)))
            `(rainbow-delimiters-depth-8-face ((,class :foreground ,str)))
            `(rainbow-delimiters-unmatched-face ((,class :foreground ,err :overline t)))
            `(rainbow-delimiters-mismatched-face ((,class :foreground ,err :overline t)))

;;;;; shm
            `(shm-current-face ((,class (:background ,green-bg-s))))
            `(shm-quarantine-face ((,class (:background ,red-bg-s))))

;;;;; show-paren
            `(show-paren-match ((,class (:background ,green-bg-s))))
            `(show-paren-mismatch ((,class (:background ,red-bg-s))))

;;;;; smartparens
            `(sp-pair-overlay-face ((,class (:background ,highlight :foreground nil))))
            `(sp-show-pair-match-face ((,class (:foreground ,mat :inherit bold :underline t))))

;;;;; spaceline
            `(spaceline-python-venv ((,class (:foreground ,comp))))
            `(spaceline-flycheck-error  ((,class (:foreground ,err))))
            `(spaceline-flycheck-info   ((,class (:foreground ,keyword))))
            `(spaceline-flycheck-warning((,class (:foreground ,war))))

;;;;; yxl-specific
            `(yxl-transient-state-title-face ((,class (:background nil :foreground ,comp :box nil :inherit bold))))

;;;;; swiper
            `(swiper-line-face ((,class (:background ,highlight :inherit bold))))
            `(swiper-match-face-1 ((,class (:inherit bold))))
            `(swiper-match-face-2 ((,class (:foreground ,head1 :underline t))))
            `(swiper-match-face-3 ((,class (:foreground ,head4 :underline t))))

;;;;; tabbar
            `(tabbar-default ((,class (:background ,bg1 :foreground ,head1 :height 0.9))))
            `(tabbar-button ((,class (:inherit tabbar-default))))
            `(tabbar-button-highlight ((,class (:inherit tabbar-default))))
            `(tabbar-highlight ((,class (:underline t))))
            `(tabbar-selected ((,class (:inherit tabbar-default :foreground ,func :weight bold))))
            `(tabbar-separator ((,class (:inherit tabbar-default))))
            `(tabbar-unselected ((,class (:inherit tabbar-default :background ,bg1 :slant italic :weight light))))
            `(swiper-match-face-4 ((,class (:foreground ,head3 :underline t))))

;;;;; term
            `(term ((,class (:foreground ,base :background ,bg1))))
            `(term-color-black ((,class (:foreground ,bg4))))
            `(term-color-blue ((,class (:foreground ,keyword))))
            `(term-color-cyan ((,class (:foreground ,cyan))))
            `(term-color-green ((,class (:foreground ,green))))
            `(term-color-magenta ((,class (:foreground ,magenta))))
            `(term-color-red ((,class (:foreground ,red))))
            `(term-color-white ((,class (:foreground ,base))))
            `(term-color-yellow ((,class (:foreground ,yellow))))

;;;;; web-mode
            `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
            `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
            `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
            `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
            `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
            `(web-mode-html-attr-name-face ((,class (:foreground ,func))))
            `(web-mode-html-attr-value-face ((,class (:foreground ,keyword))))
            `(web-mode-html-tag-face ((,class (:foreground ,keyword))))
            `(web-mode-keyword-face ((,class (:foreground ,keyword))))
            `(web-mode-string-face ((,class (:foreground ,str))))
            `(web-mode-symbol-face ((,class (:foreground ,type))))
            `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
            `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))

;;;;; which-key
            `(which-key-command-description-face ((,class (:foreground ,base))))
            `(which-key-group-description-face ((,class (:foreground ,keyword))))
            `(which-key-key-face ((,class (:foreground ,func :inherit bold))))
            `(which-key-separator-face ((,class (:background nil :foreground ,str))))
            `(which-key-special-key-face ((,class (:background ,func :foreground ,bg1))))

;;;;; which-function-mode
            `(which-func ((,class (:foreground ,func))))

;;;;; whitespace-mode
            `(whitespace-empty ((,class (:background nil :foreground ,yellow))))
            `(whitespace-indentation ((,class (:background nil :foreground ,war))))
            `(whitespace-line ((,class (:background nil :foreground ,comp))))
            `(whitespace-newline ((,class (:background nil :foreground ,comp))))
            `(whitespace-space ((,class (:background nil :foreground ,act2))))
            `(whitespace-space-after-tab ((,class (:background nil :foreground ,yellow))))
            `(whitespace-space-before-tab ((,class (:background nil :foreground ,yellow))))
            `(whitespace-tab ((,class (:background nil))))
            `(whitespace-trailing ((,class (:background ,err :foreground ,war))))

;;;;; whitespace-mode
            `(w3m-anchor ((t (:foreground ,keyword))))
            `(w3m-arrived-anchor ((t (:foreground ,const))))
            `(w3m-session-select ((t (:foreground ,comment))))
            `(w3m-session-selected ((t (:foreground ,base :bold t))))
            `(w3m-tab-background ((t (:background ,bg1 :foreground ,comment))))
            `(w3m-tab-selected-background ((t (:background ,bg1 :foreground ,base))))
            `(w3m-tab-mouse ((t (:background ,bg1 :foreground ,comment))))
            `(w3m-tab-selected ((t (:background ,bg1 :foreground ,base :bold t))))
            `(w3m-tab-unselected ((t (:background ,bg1 :foreground ,comment))))
            `(w3m-tab-selected-retrieving ((t (:background ,bg1 :foreground ,suc))))
            `(w3m-tab-unselected-retrieving ((t (:background ,bg1 :foreground ,suc))))
            `(w3m-tab-unselected-unseen ((t (:background ,bg1 :foreground ,comment))))

;;;;; other, need more work
            `(ac-completion-face ((,class (:underline t :foreground ,keyword))))
            `(ffap ((,class (:foreground ,base))))
            `(flx-highlight-face ((,class (:foreground ,comp :underline nil))))
            `(icompletep-determined ((,class :foreground ,keyword)))
            `(js2-external-variable ((,class (:foreground ,comp))))
            `(js2-function-param ((,class (:foreground ,const))))
            `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,str))))
            `(js2-jsdoc-html-tag-name ((,class (:foreground ,keyword))))
            `(js2-jsdoc-value ((,class (:foreground ,str))))
            `(js2-private-function-call ((,class (:foreground ,const))))
            `(js2-private-member ((,class (:foreground ,base))))
            `(js3-error-face ((,class (:underline ,war))))
            `(js3-external-variable-face ((,class (:foreground ,var))))
            `(js3-function-param-face ((,class (:foreground ,keyword))))
            `(js3-instance-member-face ((,class (:foreground ,const))))
            `(js3-jsdoc-tag-face ((,class (:foreground ,keyword))))
            `(js3-warning-face ((,class (:underline ,keyword))))
            `(slime-repl-inputed-output-face ((,class (:foreground ,comp))))
            `(trailing-whitespace ((,class :foreground nil :background ,err)))
            `(undo-tree-visualizer-current-face ((,class :foreground ,keyword)))
            `(undo-tree-visualizer-default-face ((,class :foreground ,base)))
            `(undo-tree-visualizer-register-face ((,class :foreground ,comp)))
            `(undo-tree-visualizer-unmodified-face ((,class :foreground ,var))))

           (custom-theme-set-variables
            theme-name
            `(ansi-color-names-vector [,bg4 ,red ,green ,yellow ,blue ,magenta ,cyan ,base]))

           ))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'yxl-common)

;; Local Variables:
;; no-byte-compile: t
;; eval: (when (featurep 'parinfer) (parinfer-mode-disable))
;; End:

;;; yxl-common.el ends here
