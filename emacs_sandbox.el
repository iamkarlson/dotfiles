(require 'pp)

(defun my/generate-project-groups ()
  "Generate dynamic org-super-agenda groups for files in projects/ with :ORDERING: property."
  (let ((project-dir (expand-file-name "projects/" org-directory))
        groups)
    (dolist (file (directory-files-recursively project-dir "\\.org$"))
      (message "Checking file: %s" file)
      ;;(message "Category: %s, Ordering: %s" category ordering)

      (with-temp-buffer
        (insert-file-contents file)
        (let ((ordering (progn
                          (goto-char (point-min))
                          (or (when (re-search-forward "^#\\:ORDERING: +\\([0-9]+\\)" nil t)
                                (string-to-number (match-string 1)))
                              1000))))
          (when ordering

            (message "File: %s, Ordering: %s" file ordering)
            (push `(:name ,file
                    :order ,ordering
                    :predicate
                    ,(lambda (item)
                       (let ((marker (get-text-property 0 'org-hd-marker item)))
                         (and marker
                              (string-prefix-p ,(expand-file-name file)
                                               (or (buffer-file-name (marker-buffer marker)) ""))))))
                  groups)))))
    ;;(reverse groups)
    (message "%s" (pp-to-string groups))
    )) ;; Keep order from filesystem

(my/generate-project-groups)

