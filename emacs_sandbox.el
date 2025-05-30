;;; -*- lexical-binding: t; -*-

(message "Debug start %s" (current-time-string))
(require 'org)
(require 'org-element)
(require 'pp)

(defun my/file-ordering (file &optional default)
  "Return numeric ORDERING property found in FILE.
If the file has no #+PROPERTY: ORDERING <n> line, return DEFAULT
(or 10 if DEFAULT is nil)."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (if (re-search-forward
         "^#\\+PROPERTY:[ \t]+\\(?:[^ \t]+[ \t]+\\)*ORDERING[ \t]+\\([0-9]+\\)"
         nil t)
        (string-to-number (match-string 1))
      (or default 200))))

(defun my/generate-project-groups ()
  "Return a list of org-super-agenda groups for every file in projects/."
  (let* ((project-dir (expand-file-name "projects/" org-directory))
         (org-directory (file-name-as-directory project-dir))
         groups)
    (dolist (file (directory-files-recursively project-dir "\\.org$"))
      (let* ((order   (my/file-ordering file 10))
             (name    (file-name-base file))
             (f       file))              ; close over FILE safely
        (push (list :name name
                    :order order
                    :pred
                    (lambda (item)
                      (let ((m (get-text-property 0 'org-hd-marker item)))
                        (when m
                          (equal (expand-file-name f)
                                 (buffer-file-name (marker-buffer m)))))))
              groups)))
    ;; org-super-agenda uses the lowest :order first; sort for sanity
    (sort groups (lambda (a b) (< (plist-get a :order)
                                  (plist-get b :order))))
    (message "Final groups:\n%s" (pp-to-string groups))
    groups
    ))


(my/generate-project-groups)

(message "Debug end %s" (current-time-string))
