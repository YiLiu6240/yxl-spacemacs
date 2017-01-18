(setq ov-highlighter-packages '(ov
                                (ov-highlighter :location site)
                                (org-ov-highlighter :location site)))

(defun ov-highlighter/init-ov ()
  (use-package ov
     :defer t))

(defun ov-highlighter/init-ov-highlighter ()
  (use-package ov-highlighter
    :config
    (progn
      (setq ov-highlight-disable-save t)
      ;; Gruvbox scheme
      (ov-make-highlight "red" '(:background "#9d0006"))
      (ov-make-highlight "green" '(:background "#79740e"))
      (ov-make-highlight "yellow" '(:background "#b57614"))
      (ov-make-highlight "blue" '(:background "#076678"))
      (ov-make-highlight "purple" '(:background "#8f3f71"))
      (ov-make-highlight "aqua" '(:background "#427b58"))
      (ov-make-highlight "orange" '(:background "#af3a03"))
      (defhydra yxl-ov-highlighter (:hint nil :color blue)
        "
 | ^Color^     | ^Markup^       | ^Font^           | ^Else^
 | _r_: red    | _c_: custom bg | _[_: decrease    | _k_: clear
 | _g_: green  | _f_: custom fg | _]_: increase    | _K_: clear all
 | _y_: yellow | _B_: Bold      | _F_: custom font |
 | _b_: blue   | _I_: Italic    | ^^               |
 | _p_: purple | _U_: Underline | ^^               |
 | _a_: aqua   | _S_: Strike    | ^^               |
 | _o_: orange | _X_: box       | ^^               |
"
        ("r" ov-highlight-red)
        ("g" ov-highlight-green)
        ("y" ov-highlight-yellow)
        ("b" ov-highlight-blue)
        ("p" ov-highlight-purple)
        ("a" ov-highlight-aqua)
        ("o" ov-highlight-orange)

        ("c" ov-highlight-color)
        ("f" ov-highlight-foreground)

        ("[" ov-highlight-decrease-font-size :color red)
        ("]" ov-highlight-increase-font-size :color red)
        ("F" ov-highlight-font)

        ("B" ov-highlight-bold)
        ("I" ov-highlight-italic)
        ("U" ov-highlight-underline)
        ("S" ov-highlight-strikethrough)
        ("X" ov-highlight-box)

        ("k" ov-highlight-clear)
        ("K" ov-highlight-clear-all)
        ("q" nil "quit")))))

;; TODO: org-ov-highlighter
