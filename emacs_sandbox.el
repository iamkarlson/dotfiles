;;; -*- lexical-binding: t; -*-

(message "Debug start %s" (current-time-string))

;;; auto-fold DONE families ───────────────────────────────────────────
(after! org
  (require 'cl-lib)

  ;; Return t when subtree has ≥1 TODO child and they’re all DONE
  (defun my/org-all-children-done-p ()
    (save-excursion
      (org-back-to-heading t)
      (let ((open 0) (seen nil))
        (org-map-entries
         (lambda ()
           (when-let ((state (org-get-todo-state)))
             (setq seen t)
             (unless (org-entry-is-done-p)
               (cl-incf open))))
         nil 'tree)
        (and seen (= open 0)))))

  (defun my/org-fold-done-on-load ()
    "Fold subtrees whose tasks are all DONE—runs once per new buffer."
    (when buffer-file-name                      ; skip brand-new unsaved buffers
      (save-excursion
        (org-with-wide-buffer
         (goto-char (point-min))
         (while (re-search-forward org-heading-regexp nil t)
           (when (my/org-all-children-done-p)
             (org-fold-hide-subtree)))))))

  ;; register the helper *after* Org is loaded
  (add-hook 'org-mode-hook #'my/org-fold-done-on-load)


  )

;; No more agenda-file nagging
(setq org-agenda-skip-unavailable-files t)

(message "Debug end %s" (current-time-string))
