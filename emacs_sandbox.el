;;; -*- lexical-binding: t; -*-

(message "Debug start %s" (current-time-string))

(defun +org/fold-done-headings-on-load ()
  (org-map-entries #'+org/toggle-fold-when-all-done nil 'file))

(message "Debug end %s" (current-time-string))
