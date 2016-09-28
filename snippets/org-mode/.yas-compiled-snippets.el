;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
                     '(("title" "#+TITLE: ${1:title}" "title"
                        (=
                         (current-column)
                         5)
                        nil nil "/Users/lysender61/.spacemacs.d/snippets/org-mode/title" nil nil)
                       ("sta" "#+STARTUP: ${1:options}" "startup"
                        (or
                         (=
                          (current-column)
                          3)
                         (=
                          (current-column)
                          0))
                        nil nil "/Users/lysender61/.spacemacs.d/snippets/org-mode/startup" nil nil)
                       ("src" "#+begin_src $1\n$2\n#+end_src" "source" nil nil nil "/Users/lysender61/.spacemacs.d/snippets/org-mode/source" nil nil)
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
                        "/Users/lysender61/.spacemacs.d/snippets/org-mode/block" nil nil)
                       ("bbs" "#+begin_src $1 :session :exports both :results output code\n$0\n#+end_src\n" "babel_session" nil nil nil "/Users/lysender61/.spacemacs.d/snippets/org-mode/babel_session" nil nil)
                       ("bbn" "#+begin_src $1 :exports both :results output code\n$0\n#+end_src\n" "babel_nosession" nil nil nil "/Users/lysender61/.spacemacs.d/snippets/org-mode/babel_nosession" nil nil)))


;;; Do not edit! File generated at Wed Sep 28 16:04:44 2016
