;;; ../src/dotfiles/doomemacs/org-roam.el -*- lexical-binding: t; -*-

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/braindb/")

(defun my/org-roam-preview-other-window ()
  (interactive)
  (org-roam-preview-visit
   (org-roam-buffer-file-at-point 'assert)
   (oref (magit-current-section) point)
   :other-window))


(use-package org-roam
  :init
    (setq org-roam-v2-ack t)
  :custom
    (org-roam-directory "~/braindb")
    (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         ;; Also see leader binding below for "SPC d" menu
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap (
        ("C-c n d" . org-roam-dailies-map)
  )
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode)
)

;;(map! :leader
      ;;"d"  org-roam-dailies-map
;;)


;;(define-key org-roam-mode-map [mouse-1] #'my/org-roam-preview-other-window)
