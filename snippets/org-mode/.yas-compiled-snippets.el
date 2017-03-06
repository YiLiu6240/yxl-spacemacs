;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
                     '(("title" "#+TITLE: ${1:title}" "title"
                        (=
                         (current-column)
                         5)
                        nil nil "/home/yiliu/.spacemacs.d/snippets/org-mode/title" nil nil)
                       ("sta" "#+STARTUP: ${1:options}" "startup"
                        (or
                         (=
                          (current-column)
                          3)
                         (=
                          (current-column)
                          0))
                        nil nil "/home/yiliu/.spacemacs.d/snippets/org-mode/startup" nil nil)
                       ("src" "#+begin_src $1\n$2\n#+end_src" "source" nil nil nil "/home/yiliu/.spacemacs.d/snippets/org-mode/src" nil nil)
                       ("node_folded" "*** desc\n:PROPERTIES:\n:VISIBILITY: folded\n:END:" "node_foled" nil nil nil "/home/yiliu/.spacemacs.d/snippets/org-mode/node_folded" nil nil)
                       ("block" "#+BEGIN_$1 $2\n  $0\n#+END_$1" "#+begin_...#+end_"
                        (or
                         (=
                          (current-column)
                          5)
                         (=
                          (current-column)
                          0))
                        nil
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/home/yiliu/.spacemacs.d/snippets/org-mode/block" nil nil)
                       ("bbs" "#+begin_src $1 :session :exports both :results output code\n$0\n#+end_src\n" "babel_session" nil nil nil "/home/yiliu/.spacemacs.d/snippets/org-mode/babel_session" nil nil)
                       ("bbn" "#+begin_src $1 :exports both :results output code\n$0\n#+end_src\n" "babel_nosession" nil nil nil "/home/yiliu/.spacemacs.d/snippets/org-mode/babel_nosession" nil nil)))


;;; Do not edit! File generated at Mon Mar  6 10:57:29 2017
