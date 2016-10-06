(defmacro dyn-let (varlist fn setfaces setvars)
  (list 'let (append varlist (funcall fn)) setfaces setvars))

(defgroup yxl-theme nil
  "Spacemacs-theme options."
  :group 'faces)

(defcustom yxl-theme-comment-bg t
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
  (dyn-let ((class '((class color) (min-colors 89))) ;;              ~~ Dark ~~                              ~~ Light ~~
        ;;                                                          GUI       TER                           GUI       TER
        ;; generic
        ;; mode line active color 1
        (act1          (if (eq variant 'dark) (if (true-color-p) "#132126" "#121212") (if (true-color-p) "#e7e5eb" "#d7dfff")))
        ;; mode line active color 2
        (act2          (if (eq variant 'dark) (if (true-color-p) "#073642" "#444444") (if (true-color-p) "#d3d3e7" "#afafd7")))
        ;; basic foreground color
        (base          (if (eq variant 'dark) (if (true-color-p) "#839496" "#b2b2b2") (if (true-color-p) "#655370" "#5f5f87")))
        ;; (base          (if (eq variant 'dark) (if (true-color-p) "#74878c" "#b2b2b2") (if (true-color-p) "#655370" "#5f5f87")))
        ;; dimmed version of foreground color
        (base-dim      (if (eq variant 'dark) (if (true-color-p) "#545557" "#585858") (if (true-color-p) "#cdc5c8" "#afafd7")))
        ;; normal background color
        ;; (bg1           (if (eq variant 'dark) (if (true-color-p) "#132126" "#262626") (if (true-color-p) "#fbf8ef" "#ffffff")))
        (bg1           (if (eq variant 'dark) (if (true-color-p) "#15262c" "#262626") (if (true-color-p) "#fbf8ef" "#ffffff")))
        ;; current line highlight
        (bg2           (if (eq variant 'dark) (if (true-color-p) "#132b34" "#1c1c1c") (if (true-color-p) "#efeae9" "#e4e4e4")))
        (bg2-hl        (if (eq variant 'dark) (if (true-color-p) "#1b343d" "#1c1c1c") (if (true-color-p) "#efeae9" "#e4e4e4")))
        ;; darker shade of bg1
        (bg3           (if (eq variant 'dark) (if (true-color-p) "#073642" "#121212") (if (true-color-p) "#e3dedd" "#d0d0d0")))
        ;; darkeest of bg
        (bg4           (if (eq variant 'dark) (if (true-color-p) "#073642" "#080808") (if (true-color-p) "#d2ceda" "#bcbcbc")))
        ;; mode line border color
        (border        (if (eq variant 'dark) (if (true-color-p) "#073642" "#111111") (if (true-color-p) "#b3b9be" "#b3b9be")))
        ;; code block fg
        (cblk          (if (eq variant 'dark) (if (true-color-p) "#cbc1d5" "#b2b2b2") (if (true-color-p) "#655370" "#5f5f87")))
        ;; code block bg
        (cblk-bg       (if (eq variant 'dark) (if (true-color-p) "#2f2b33" "#262626") (if (true-color-p) "#e8e3f0" "#ffffff")))
        ;; code block header line
        (cblk-ln       (if (eq variant 'dark) (if (true-color-p) "#827591" "#af5faf") (if (true-color-p) "#9380b2" "#af5fdf")))
        ;; code block header line bg
        (cblk-ln-bg    (if (eq variant 'dark) (if (true-color-p) "#373040" "#333333") (if (true-color-p) "#ddd8eb" "#dfdfff")))
        ;; cursor/point ???
        (cursor        (if (eq variant 'dark) (if (true-color-p) "#e3dedd" "#d0d0d0") (if (true-color-p) "#100a14" "#121212")))
        ;; constant
        (const         (if (eq variant 'dark) (if (true-color-p) "#33859e" "#d75fd7") (if (true-color-p) "#4e3163" "#8700af")))
        ;; comment
        (comment       (if (eq variant 'dark) (if (true-color-p) "#586e75" "#008787") (if (true-color-p) "#2aa1ae" "#008787")))
        ;; comment bg
        (comment-bg    (if (eq variant 'dark) (if (true-color-p) "#15262c" "#262626") (if (true-color-p) "#ecf3ec" "#ffffff")))
        ;; complementary
        (comp          (if (eq variant 'dark) (if (true-color-p) "#b58900" "#d75fd7") (if (true-color-p) "#6c4173" "#8700af")))
        ;; errors
        (err           (if (eq variant 'dark) (if (true-color-p) "#dc322f" "#e0211d") (if (true-color-p) "#e0211d" "#e0211d")))
        ;; functions
        (func          (if (eq variant 'dark) (if (true-color-p) "#33859e" "#d75fd7") (if (true-color-p) "#6c3163" "#8700af")))
        ;; org lv1 heading
        (head1         (if (eq variant 'dark) (if (true-color-p) "#b58900" "#875f00") (if (true-color-p) "#3a81c3" "#33859e")))
        (head1-bg      (if (eq variant 'dark) (if (true-color-p) "#132b34" "#262626") (if (true-color-p) "#edf1ed" "#ffffff")))
        ;; org lv2 heading
        (head2         (if (eq variant 'dark) (if (true-color-p) "#2aa889" "#2aa198") (if (true-color-p) "#2d9574" "#2aa198")))
        (head2-bg      (if (eq variant 'dark) (if (true-color-p) "#293235" "#262626") (if (true-color-p) "#edf2e9" "#ffffff")))
        ;; org lv3 heading
        (head3         (if (eq variant 'dark) (if (true-color-p) "#33859e" "#67b11d") (if (true-color-p) "#67b11d" "#5faf00")))
        (head3-bg      (if (eq variant 'dark) (if (true-color-p) "#293235" "#262626") (if (true-color-p) "#edf2e9" "#ffffff")))
        ;; org lv4 heading
        (head4         (if (eq variant 'dark) (if (true-color-p) "#99d1ce" "#33859e") (if (true-color-p) "#b1951d" "#875f00")))
        (head4-bg      (if (eq variant 'dark) (if (true-color-p) "#32322c" "#262626") (if (true-color-p) "#f6f1e1" "#ffffff")))
        ;; highlighted area ???
        (highlight     (if (eq variant 'dark) (if (true-color-p) "#1d4250" "#444444") (if (true-color-p) "#d3d3e7" "#d7d7ff")))
        ;; keyword
        ;; (keyword       (if (eq variant 'dark) (if (true-color-p) "#859900" "#33859e") (if (true-color-p) "#3a81c3" "#33859e")))
        (keyword       (if (eq variant 'dark) (if (true-color-p) "#679a01" "#33859e") (if (true-color-p) "#3a81c3" "#33859e")))
        ;; line numbers
        (lnum          (if (eq variant 'dark) (if (true-color-p) "#44505c" "#444444") (if (true-color-p) "#a8a8bf" "#af87af")))
        ;; matched, matching parens, brackets, tags
        (mat           (if (eq variant 'dark) (if (true-color-p) "#86dc2f" "#86dc2f") (if (true-color-p) "#ba2f59" "#af005f")))
        ;; meta, org's meta line
        (meta          (if (eq variant 'dark) (if (true-color-p) "#6c71c4" "#af875f") (if (true-color-p) "#da8b55" "#df5f5f")))
        ;; string
        (str           (if (eq variant 'dark) (if (true-color-p) "#2aa198" "#2aa198") (if (true-color-p) "#2d9574" "#2aa198")))
        ;; success
        (suc           (if (eq variant 'dark) (if (true-color-p) "#86dc2f" "#86dc2f") (if (true-color-p) "#42ae2c" "#00af00")))
        ;; tooltip
        (ttip          (if (eq variant 'dark) (if (true-color-p) "#9a9aba" "#888888") (if (true-color-p) "#8c799f" "#5f5f87")))
        ;; tooltip selection
        (ttip-sl       (if (eq variant 'dark) (if (true-color-p) "#5e5079" "#333333") (if (true-color-p) "#c8c6dd" "#afafff")))
        ;; tooltip bg
        (ttip-bg       (if (eq variant 'dark) (if (true-color-p) "#34323e" "#444444") (if (true-color-p) "#e2e0ea" "#dfdfff")))
        ;; type
        (type          (if (eq variant 'dark) (if (true-color-p) "#b58900" "#df005f") (if (true-color-p) "#ba2f59" "#af005f")))
        ;; variable
        (var           (if (eq variant 'dark) (if (true-color-p) "#33859e" "#8787d7") (if (true-color-p) "#715ab1" "#af5fd7")))
        ;; warning
        (war           (if (eq variant 'dark) (if (true-color-p) "#dc752f" "#dc752f") (if (true-color-p) "#dc752f" "#dc752f")))

        ;; colors
        (aqua          (if (eq variant 'dark) (if (true-color-p) "#2d9574" "#2aa198") (if (true-color-p) "#2d9574" "#2aa198")))
        (aqua-bg       (if (eq variant 'dark) (if (true-color-p) "#293235" "#262626") (if (true-color-p) "#edf2e9" "#ffffff")))
        (green         (if (eq variant 'dark) (if (true-color-p) "#859900" "#67b11d") (if (true-color-p) "#67b11d" "#5faf00")))
        (green-bg      (if (eq variant 'dark) (if (true-color-p) "#293235" "#262626") (if (true-color-p) "#edf2e9" "#ffffff")))
        (green-bg-s    (if (eq variant 'dark) (if (true-color-p) "#29422d" "#262626") (if (true-color-p) "#dae6d0" "#ffffff")))
        (cyan          (if (eq variant 'dark) (if (true-color-p) "#2aa889" "#00ffff") (if (true-color-p) "#21b8c7" "#008080")))
        (orange        (if (eq variant 'dark) (if (true-color-p) "#cb4b16" "#cb4b16") (if (true-color-p) "#cb4b16" "#cb4b16")))
        (red           (if (eq variant 'dark) (if (true-color-p) "#dc322f" "#d70000") (if (true-color-p) "#f2241f" "#d70008")))
        (red-bg        (if (eq variant 'dark) (if (true-color-p) "#3c2a2c" "#262626") (if (true-color-p) "#faede4" "#ffffff")))
        (red-bg-s      (if (eq variant 'dark) (if (true-color-p) "#512e31" "#262626") (if (true-color-p) "#eed9d2" "#ffffff")))
        (blue          (if (eq variant 'dark) (if (true-color-p) "#4f97d7" "#33859e") (if (true-color-p) "#3a81c3" "#33859e")))
        (blue-bg       (if (eq variant 'dark) (if (true-color-p) "#293239" "#262626") (if (true-color-p) "#edf1ed" "#d7d7ff")))
        ;; TODO: add violet
        (magenta       (if (eq variant 'dark) (if (true-color-p) "#a31db1" "#af00df") (if (true-color-p) "#a31db1" "#800080")))
        (yellow        (if (eq variant 'dark) (if (true-color-p) "#b58900" "#875f00") (if (true-color-p) "#b1951d" "#875f00")))
        (yellow-bg     (if (eq variant 'dark) (if (true-color-p) "#32322c" "#262626") (if (true-color-p) "#f6f1e1" "#ffffff")))
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
     `(font-lock-builtin-face ((,class (:foreground ,keyword))))
     `(font-lock-comment-face ((,class (:foreground ,comment :background ,(when yxl-theme-comment-bg comment-bg)))))
     `(font-lock-constant-face ((,class (:foreground ,const))))
     `(font-lock-doc-face ((,class (:foreground ,comment))))
     `(font-lock-function-name-face ((,class (:foreground ,func :inherit bold))))
     `(font-lock-keyword-face ((,class (:inherit bold :foreground ,keyword))))
     `(font-lock-negation-char-face ((,class (:foreground ,const))))
     `(font-lock-preprocessor-face ((,class (:foreground ,func))))
     `(font-lock-reference-face ((,class (:foreground ,const))))
     `(font-lock-string-face ((,class (:foreground ,str))))
     `(font-lock-type-face ((,class (:foreground ,type :inherit bold))))
     `(font-lock-variable-name-face ((,class (:foreground ,var))))
     `(font-lock-warning-face ((,class (:foreground ,war :background ,bg1))))
     `(fringe ((,class (:background ,bg1 :foreground ,base))))
     `(header-line ((,class :background ,bg4)))
     `(highlight ((,class (:foreground ,base :background ,highlight))))
     `(hl-line ((,class (:background ,bg2-hl))))
     `(isearch ((,class (:foreground ,bg1 :background ,mat))))
     `(lazy-highlight ((,class (:background ,blue-bg :weight normal))))
     `(link ((,class (:foreground ,comment :underline t))))
     `(link-visited ((,class (:foreground ,comp :underline t))))
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
     `(avy-lead-face   ((,class (:background ,blue-bg :foreground ,magenta))))
     `(avy-lead-face-0 ((,class (:background ,blue-bg :foreground ,blue))))
     `(avy-lead-face-1 ((,class (:background ,blue-bg :foreground ,magenta))))
     `(avy-lead-face-2 ((,class (:background ,blue-bg :foreground ,blue))))

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
     ;; `(company-echo-common ((,class (:background ,base :foreground ,bg1))))
     ;; `(company-preview ((,class (:background ,ttip-bg :foreground ,ttip))))
     ;; `(company-preview-common ((,class (:background ,ttip-bg :foreground ,base))))
     ;; `(company-preview-search ((,class (:inherit match))))
     ;; `(company-scrollbar-bg ((,class (:background ,bg2))))
     ;; `(company-scrollbar-fg ((,class (:background ,act2))))
     ;; `(company-template-field ((,class (:inherit region))))
     ;; `(company-tooltip ((,class (:background ,ttip-bg :foreground ,ttip))))
     ;; `(company-tooltip-annotation ((,class (:foreground ,keyword))))
     ;; `(company-tooltip-common ((,class (:background ,ttip-bg :foreground ,base))))
     ;; `(company-tooltip-common-selection ((,class (:foreground ,base))))
     ;; `(company-tooltip-mouse ((,class (:inherit highlight))))
     ;; `(company-tooltip-search ((,class (:inherit match))))
     ;; `(company-tooltip-selection ((,class (:background ,ttip-sl :foreground ,base))))
     '(company-echo-common ((t (:background "#839496" :foreground "#132126"))))
     '(company-preview ((t (:background "#002b36" :foreground "#599cab"))))
     '(company-preview-common ((t (:background "#002b36" :foreground "#839496"))))
     '(company-preview-search ((t (:inherit match))))
     '(company-scrollbar-bg ((t (:background "#132b34"))))
     '(company-scrollbar-fg ((t (:background "#203841"))))
     '(company-template-field ((t (:inherit region))))
     '(company-tooltip ((t (:background "#002b36" :foreground "#599cab"))))
     '(company-tooltip-annotation ((t (:foreground "#859900"))))
     '(company-tooltip-common ((t (:background "#002b36" :foreground "#839496"))))
     '(company-tooltip-common-selection ((t (:foreground "#839496"))))
     '(company-tooltip-mouse ((t (:inherit highlight))))
     '(company-tooltip-search ((t (:inherit match))))
     '(company-tooltip-selection ((t (:background "#246361" :foreground "#839496"))))

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
     `(dired-directory ((,class (:foreground ,keyword :background ,bg1 :inherit bold))))
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
     `(helm-selection ((,class (:background ,highlight))))
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
     `(hi-yellow ((,class (:foreground ,yellow :background ,yellow-bg))))
     `(hi-green  ((,class (:foreground ,green :background ,green-bg))))

;;;;; highlight-indentation
     `(highlight-indentation-face ((,class (:background ,comment-bg))))
     `(highlight-indentation-current-column-face ((,class (:background ,bg2))))

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
     `(ivy-current-match ((,class (:background ,highlight :inherit bold))))
     `(ivy-minibuffer-match-face-1 ((,class (:inherit bold))))
     `(ivy-minibuffer-match-face-2 ((,class (:foreground ,head1 :underline t))))
     `(ivy-minibuffer-match-face-3 ((,class (:foreground ,head4 :underline t))))
     `(ivy-minibuffer-match-face-4 ((,class (:foreground ,head3 :underline t))))
     `(ivy-remote ((,class (:foreground ,cyan))))

;;;;; latex
     `(font-latex-math-face ((,class (:foreground ,type))))
     `(font-latex-verbatim-face ((,class (:inherit font-lock-string-face))))
     `(font-latex-bold-face ((,class (:foreground ,comp))))
     `(font-latex-italic-face ((,class (:foreground ,keyword :italic t))))
     `(font-latex-match-reference-keywords ((,class (:foreground ,const))))
     `(font-latex-match-variable-keywords ((,class (:foreground ,var))))
     ;; `(font-latex-sectioning-0-face ((,class (:inherit bold :foreground ,head3 :height ,(if yxl-theme-org-height 1.3 1.0) :background ,(when yxl-theme-org-highlight head3-bg)))))
     ;; `(font-latex-sectioning-1-face ((,class (:inherit bold :foreground ,head4 :height ,(if yxl-theme-org-height 1.3 1.0) :background ,(when yxl-theme-org-highlight head4-bg)))))
     ;; `(font-latex-sectioning-2-face ((,class (:inherit bold :foreground ,head1 :height ,(if yxl-theme-org-height 1.3 1.0) :background ,(when yxl-theme-org-highlight head1-bg)))))
     ;; `(font-latex-sectioning-3-face ((,class (:inherit bold :foreground ,head2 :height ,(if yxl-theme-org-height 1.2 1.0) :background ,(when yxl-theme-org-highlight head2-bg)))))
     ;; `(font-latex-sectioning-4-face ((,class (:bold nil :foreground ,head3 :height ,(if yxl-theme-org-height 1.1 1.0) :background ,(when yxl-theme-org-highlight head3-bg)))))
     ;; `(font-latex-sectioning-5-face ((,class (:bold nil :foreground ,head4 :background ,(when yxl-theme-org-highlight head4-bg)))))
     ;; part
     `(font-latex-sectioning-0-face ((,class (:inherit font-latex-sectioning-2-face))))
     ;; chapter
     `(font-latex-sectioning-1-face ((,class (:inherit font-latex-sectioning-2-face))))
     ;; section
     `(font-latex-sectioning-2-face ((,class (:foreground ,head1, :weight bold))))
     ;; subsection
     `(font-latex-sectioning-3-face ((,class (:foreground ,head2 :weight bold))))
     ;; subsubsection
     `(font-latex-sectioning-4-face ((,class (:foreground ,head3 :weight bold))))
     ;; paragraph
     `(font-latex-sectioning-5-face ((,class (:inherit font-latex-sectioning-6-face :weight bold))))
     `(font-latex-sectioning-6-face ((,class (:foreground ,head4))))
     `(font-latex-string-face ((,class (:foreground ,str))))
     `(font-latex-sedate-face ((,class (:foreground ,comment))))
     `(font-latex-warning-face ((,class (:foreground ,meta))))

;;;;; linum-mode
     `(linum ((,class (:foreground ,lnum :background ,bg2))))

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

;;;;; man
     `(Man-overstrike ((,class (:foreground ,head1 :inherit bold))))
     `(Man-reverse ((,class (:foreground ,highlight))))
     `(Man-underline ((,class (:foreground ,comp :underline t))))

;;;;; markdown
     `(markdown-header-face-1 ((,class (:foreground ,head1 :weight bold))))
     `(markdown-header-face-2 ((,class (:foreground, head2 :weight bold))))
     `(markdown-header-face-3 ((,class (:foreground ,head3 :weight bold))))
     `(markdown-header-face-4 ((,class (:foreground ,head4 :weight bold))))
     `(markdown-header-face-5 ((,class (:inherit markdown-header-face-6 :weight bold))))
     `(markdown-header-face-6 ((,class (:foreground ,head4))))

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

;;;;; org
     `(org-agenda-clocking ((,class (:background ,highlight :foreground ,comp))))
     `(org-agenda-date ((,class (:foreground ,var :height ,(if yxl-theme-org-height 1.1 1.0)))))
     `(org-agenda-date-today ((,class (:foreground ,keyword :slant italic :inherit bold :height ,(if yxl-theme-org-height 1.3 1.0)))))
     `(org-agenda-date-weekend ((,class (:inherit bold :foreground ,var))))
     `(org-agenda-done ((,class (:foreground ,suc :height ,(if yxl-theme-org-height 1.2 1.0)))))
     `(org-agenda-structure ((,class (:inherit bold :foreground ,comp))))
     `(org-block ((,class (:background ,cblk-bg :foreground ,cblk))))
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
     `(org-document-title ((,class (:foreground ,func :inherit bold :height ,(if yxl-theme-org-height 1.4 1.0) :underline t))))
     `(org-done ((,class (:foreground ,comment :inherit bold))))
     `(org-ellipsis ((,class (:foreground ,keyword))))
     `(org-footnote  ((,class (:underline t :foreground ,base))))
     `(org-hide ((,class (:foreground ,base))))
     `(org-kbd ((,class (:inherit region :foreground ,base :box (:line-width 1 :style released-button)))))
     ;; `(org-level-1 ((,class (:foreground "#edb443", :weight bold :height 1.5))))
     ;; `(org-level-2 ((,class (:foreground "#33859e", :height 1.2))))
     `(org-level-1 ((,class (:foreground ,type :weight bold :height 1.3))))
     `(org-level-2 ((,class (:foreground ,func :height 1.1))))
     `(org-level-3 ((,class (:inherit org-level-8))))
     `(org-level-4 ((,class (:inherit org-level-8))))
     `(org-level-5 ((,class (:inherit org-level-8))))
     `(org-level-6 ((,class (:inherit org-level-8))))
     `(org-level-7 ((,class (:inherit org-level-8))))
     `(org-level-8 ((,class (:foreground ,base))))
     `(org-link ((,class (:underline t :foreground ,comment))))
     `(org-meta-line ((,class (:foreground ,meta))))
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
     `(powerline-active1 ((,class (:background ,act2 :foreground ,base))))
     `(powerline-active2 ((,class (:background ,bg2 :foreground ,base))))
     `(powerline-inactive1 ((,class (:background ,bg2 :foreground ,base))))
     `(powerline-inactive2 ((,class (:background ,bg1 :foreground ,base))))

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
     '(w3m-anchor ((t (:foreground "#33859e"))))
     '(w3m-arrived-anchor ((t (:foreground "#097aca"))))
     '(w3m-session-select ((t (:foreground "#839496"))))
     '(w3m-session-selected ((t (:foreground "#93a1a1" :bold t :underline t))))
     '(w3m-tab-background ((t (:background "#132126" :foreground "#839496"))))
     '(w3m-tab-selected-background
       ((t (:background "#132126" :foreground "#839496"))))
     '(w3m-tab-mouse ((t (:background "#073642" :foreground "#b58900"))))
     '(w3m-tab-selected ((t (:background "#203841" :foreground "#93a1a1"
                                         :bold t))))
     '(w3m-tab-unselected ((t (:background "#073642" :foreground "#839496"))))
     '(w3m-tab-selected-retrieving ((t (:background "#073642" :foreground "#dc322f"))))
     '(w3m-tab-unselected-retrieving
       ((t (:background "#073642" :foreground "#cb4b16"))))
     '(w3m-tab-unselected-unseen ((t (:background "#073642" :foreground "#d33682"))))

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
;; End:

;;; yxl-common.el ends here
