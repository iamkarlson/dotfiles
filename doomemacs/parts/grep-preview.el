;; -*- lexical-binding: t; -*-
;;
;; Grep preview functionality
;; Allows previewing files from grep results in a split window
;;

(defvar-local my/grep-preview-window nil
  "Window showing grep preview.")

(defun my/grep-preview-file-at-point ()
  "Preview file at point in grep-mode by splitting window to the right.
The preview window stays open (persistent) and focus remains in grep buffer."
  (interactive)
  (condition-case err
      (let* ((grep-buffer (current-buffer))
             (grep-window (selected-window))
             ;; Save position to restore later
             (orig-point (point))
             ;; Use next-error machinery to get the location
             (next-error-buffer (current-buffer))
             target-buffer
             target-pos)

        ;; Temporarily visit the error location to get buffer and position
        (save-excursion
          (compile-goto-error)
          (setq target-buffer (current-buffer)
                target-pos (point)))

        ;; Return to grep buffer
        (switch-to-buffer grep-buffer)
        (goto-char orig-point)

        ;; Create or reuse preview window
        (unless (and (window-live-p my/grep-preview-window)
                     (not (eq my/grep-preview-window grep-window)))
          (setq my/grep-preview-window (split-window-right)))

        ;; Show file in preview window
        (with-selected-window my/grep-preview-window
          (switch-to-buffer target-buffer)
          (goto-char target-pos)
          (recenter))

        ;; Ensure focus stays in grep buffer
        (select-window grep-window))
    (error
     (message "Could not preview file: %s" (error-message-string err)))))

(defun my/grep-close-preview ()
  "Close the grep preview window if it exists."
  (interactive)
  (when (and my/grep-preview-window
             (window-live-p my/grep-preview-window))
    (delete-window my/grep-preview-window)
    (setq my/grep-preview-window nil)))

;; Add keybindings for both grep-mode and wgrep-mode
(map! :after grep
      :map grep-mode-map
      :n "C-o" #'my/grep-preview-file-at-point
      :n "C-q" #'my/grep-close-preview)

(map! :after wgrep
      :map wgrep-mode-map
      :n "C-o" #'my/grep-preview-file-at-point
      :n "C-q" #'my/grep-close-preview)
