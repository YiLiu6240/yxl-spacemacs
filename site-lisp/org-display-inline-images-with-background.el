(require 'org)
;; source:
;; https://gist.github.com/stardiviner/dd7f4bf5f38dfffc3afc

(defcustom org-inline-image-background "white"
  "The color used as the default background for inline images.
  When nil, use the default face background."
  :group 'org
  :type '(choice color (const nil)))

;; modified version of original `org-display-inline-images'.
(defun org-display-inline-images-with-background (&optional include-linked refresh beg end)
  "Display inline images.
An inline image is a link which follows either of these
conventions:
  1. Its path is a file with an extension matching return value
     from `image-file-name-regexp' and it has no contents.
  2. Its description consists in a single link of the previous
     type.
When optional argument INCLUDE-LINKED is non-nil, also links with
a text description part will be inlined.  This can be nice for
a quick look at those images, but it does not reflect what
exported files will look like.
When optional argument REFRESH is non-nil, refresh existing
images between BEG and END.  This will create new image displays
only if necessary.  BEG and END default to the buffer
boundaries."
  (interactive "P")
  (when (display-graphic-p)
    (unless refresh
      (org-remove-inline-images)
      (when (fboundp 'clear-image-cache) (clear-image-cache)))
    (org-with-wide-buffer
     (goto-char (or beg (point-min)))
     (let ((case-fold-search t)
           (file-extension-re (org-image-file-name-regexp)))
       (while (re-search-forward "[][]\\[\\(?:file\\|[./~]\\)" end t)
         (let ((link (save-match-data (org-element-context))))
           ;; Check if we're at an inline image.
           (when (and (equal (org-element-property :type link) "file")
                      (or include-linked
                          (not (org-element-property :contents-begin link)))
                      (let ((parent (org-element-property :parent link)))
                        (or (not (eq (org-element-type parent) 'link))
                            (not (cdr (org-element-contents parent)))))
                      (org-string-match-p file-extension-re
                                          (org-element-property :path link)))
             (let ((file (expand-file-name
                          (org-link-unescape
                           (org-element-property :path link)))))
               (when (file-exists-p file)
                 (let ((width
                        ;; Apply `org-image-actual-width' specifications.
                        (cond
                         ((not (image-type-available-p 'imagemagick)) nil)
                         ((eq org-image-actual-width t) nil)
                         ((listp org-image-actual-width)
                          (or
                           ;; First try to find a width among
                           ;; attributes associated to the paragraph
                           ;; containing link.
                           (let ((paragraph
                                  (let ((e link))
                                    (while (and (setq e (org-element-property
                                                         :parent e))
                                                (not (eq (org-element-type e)
                                                         'paragraph))))
                                    e)))
                             (when paragraph
                               (save-excursion
                                 (goto-char (org-element-property :begin paragraph))
                                 (when
                                     (re-search-forward
                                      "^[ \t]*#\\+attr_.*?: +.*?:width +\\(\\S-+\\)"
                                      (org-element-property
                                       :post-affiliated paragraph)
                                      t)
                                   (string-to-number (match-string 1))))))
                           ;; Otherwise, fall-back to provided number.
                           (car org-image-actual-width)))
                         ((numberp org-image-actual-width)
                          org-image-actual-width)))
                       (old (get-char-property-and-overlay
                             (org-element-property :begin link)
                             'org-image-overlay)))
                   (if (and (car-safe old) refresh)
                       (image-refresh (overlay-get (cdr old) 'display))
                     (let ((image (create-image file
                                                (and width 'imagemagick)
                                                nil
                                                :width width
                                                :background org-inline-image-background)))
                       (when image
                         (let* ((link
                                 ;; If inline image is the description
                                 ;; of another link, be sure to
                                 ;; consider the latter as the one to
                                 ;; apply the overlay on.
                                 (let ((parent
                                        (org-element-property :parent link)))
                                   (if (eq (org-element-type parent) 'link)
                                       parent
                                     link)))
                                (ov (make-overlay
                                     (org-element-property :begin link)
                                     (progn
                                       (goto-char
                                        (org-element-property :end link))
                                       (skip-chars-backward " \t")
                                       (point)))))
                           (overlay-put ov 'display image)
                           (overlay-put ov 'face 'default)
                           (overlay-put ov 'org-image-overlay t)
                           (overlay-put
                            ov 'modification-hooks
                            (list 'org-display-inline-remove-overlay))
                           (push ov org-inline-image-overlays)))))))))))))))

(provide 'org-display-inline-images-with-background)
