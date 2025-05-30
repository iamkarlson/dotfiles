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
  "Create one group per project file under projects/."
  (let* ((project-dir (expand-file-name "projects/" org-directory))
         groups)
    (dolist (file (directory-files-recursively project-dir "\\.org$"))
      (let* ((order (my/file-ordering file 200))
             (name  (file-name-base file)))
        (push (list :name name
                    :order order
                    :file-path (regexp-quote (expand-file-name file)))
              groups)))
    (sort groups (lambda (a b) (< (plist-get a :order)
                                  (plist-get b :order))))))

(my/generate-project-groups)

(message "Debug end %s" (current-time-string))
