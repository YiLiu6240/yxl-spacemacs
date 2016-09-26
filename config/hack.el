(defun spacemacs-buffer//insert-buttons ()
  (goto-char (point-max))
  (insert "     ")
  (spacemacs//insert--shortcut "m" "[?]" t)
  (widget-create 'url-link
                 :tag (propertize "?" 'face 'font-lock-doc-face)
                 :help-echo "Open the quickhelp."
                 :action (lambda (&rest ignore)
                           (spacemacs-buffer/toggle-note (concat spacemacs-info-directory "quickhelp.txt")
                                                         ;; if nil is returned, just delete the current note widgets
                                                         (spacemacs-buffer//insert-note-p 'quickhelp)))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 )
  (insert " ")
  (widget-create 'url-link
                 :tag (propertize "Homepage" 'face 'font-lock-keyword-face)
                 :help-echo "Open the Spacemacs Github page in your browser."
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 "http://spacemacs.org")
  (insert " ")
  (widget-create 'url-link
                 :tag (propertize "Documentation" 'face 'font-lock-keyword-face)
                 :help-echo "Open the Spacemacs documentation in your browser."
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 "http://spacemacs.org/doc/DOCUMENTATION.html")
  (insert " ")
  (widget-create 'url-link
                 :tag (propertize "Gitter Chat" 'face 'font-lock-keyword-face)
                 :help-echo "Ask questions and chat with fellow users in our chat room."
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 "https://gitter.im/syl20bnr/spacemacs")
  (insert " ")
  (widget-create 'push-button
                 :help-echo "Update Spacemacs core and layers."
                 :action (lambda (&rest ignore) (spacemacs/switch-to-version))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 (propertize "Update Spacemacs" 'face 'font-lock-keyword-face))
  (insert "\n               ")
  (widget-create 'push-button
                 :help-echo "Update all ELPA packages to the latest versions."
                 :action (lambda (&rest ignore) (configuration-layer/update-packages))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 (propertize "Update Packages" 'face 'font-lock-keyword-face))
  (insert " ")
  (widget-create 'push-button
                 :help-echo "Rollback ELPA package updates if something got borked."
                 :action (lambda (&rest ignore) (call-interactively 'configuration-layer/rollback))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 (propertize "Rollback Package Update" 'face 'font-lock-keyword-face))
  (insert "\n")
  (insert "                  ")
  (widget-create 'push-button
                 :tag (propertize "Release Notes" 'face 'font-lock-preprocessor-face)
                 :help-echo "Hide or show the Changelog"
                 :action (lambda (&rest ignore)
                           (spacemacs-buffer/toggle-note
                            (concat spacemacs-release-notes-directory
                                    spacemacs-buffer-version-info
                                    ".txt")
                            ;; if nil is returned,
                            ;; just delete the current note widgets
                            (spacemacs-buffer//insert-note-p 'release-note)))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 )
  (insert " ")
  (widget-create 'url-link
                 :tag (propertize "Search in Spacemacs" 'face 'font-lock-function-name-face)
                 :help-echo "Find Spacemacs package and layer configs using helm-spacemacs."
                 :action (lambda (&rest ignore) (call-interactively 'helm-spacemacs))
                 :mouse-face 'highlight
                 :follow-link "\C-m")
  (insert "\n\n")
  (insert "                  ")
  (insert " ")
  (widget-create 'push-button
                 :tag (propertize "yxl/frame-setup" 'face 'font-lock-comment-face)
                 :action (lambda (&rest ignore) (yxl/frame-setup))
                 :mouse-face 'highlight)
  (insert "\n\n")
  (insert "                  ")
  (insert " ")
  (widget-create 'push-button
                 :tag (propertize "Agenda: work" 'face 'font-lock-type-face)
                 :action (lambda (&rest ignore) (yxl/org-agenda-work))
                 :mouse-face 'highlight)
  (insert " ")
  (widget-create 'push-button
                 :tag (propertize "Agenda: life" 'face 'font-lock-string-face)
                 :action (lambda (&rest ignore) (yxl/org-agenda-life))
                 :mouse-face 'highlight)
  (insert "\n")
  (insert "                  ")
  (insert "   ")
  (widget-create 'push-button
                 :tag (propertize "Org: work" 'face 'font-lock-type-face)
                 :action (lambda (&rest ignore) (yxl/find-file-org-work))
                 :mouse-face 'highlight)
  (insert "    ")
  (widget-create 'push-button
                 :tag (propertize "Org: life" 'face 'font-lock-string-face)
                 :action (lambda (&rest ignore) (yxl/find-file-org))
                 :mouse-face 'highlight)
  (insert "\n\n"))
