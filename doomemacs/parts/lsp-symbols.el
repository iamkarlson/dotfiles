;; -*- lexical-binding: t; -*-

;; LSP Treemacs Symbols Configuration
;; This configures the LSP symbols outline in treemacs

(after! lsp-treemacs
  ;; Enable automatic synchronization between LSP and treemacs
  (lsp-treemacs-sync-mode 1)

  ;; Make sure symbols update automatically
  ;; The timer is created when you open lsp-treemacs-symbols
  ;; but we can configure related behaviors here

  ;; Show detailed signatures in the outline
  (setq lsp-treemacs-detailed-outline t)

  ;; Don't add space between root nodes (more compact)
  (setq lsp-treemacs-symbols-space-between-root-nodes nil)

  ;; Sort symbols by their position in the file (top to bottom)
  (setq lsp-treemacs-symbols-sort-functions '(lsp-treemacs-sort-by-position))

  ;; Configure the position of the symbols window
  (setq lsp-treemacs-symbols-position-params
        `((side . right)           ; Show on the right side
          (slot . 2)               ; Slot 2 (treemacs is slot 1)
          (window-width . 35)))    ; Width of the symbols window
  )

;; Helper function to toggle symbols view and ensure it updates
(defun my/lsp-treemacs-symbols-toggle ()
  "Toggle LSP symbols view with auto-refresh."
  (interactive)
  (if-let ((buf (get-buffer lsp-treemacs-symbols-buffer-name)))
      (if (get-buffer-window buf)
          (delete-window (get-buffer-window buf))
        (lsp-treemacs-symbols))
    (lsp-treemacs-symbols)))

;; Optional: Add a keybinding for easy access
(map! :leader
      :desc "Toggle LSP symbols outline"
      :n "c z" #'my/lsp-treemacs-symbols-toggle)

;; Enable highlighting in the symbols buffer to show where you are
(add-hook 'lsp-treemacs-symbols-mode-hook
          (lambda ()
            (hl-line-mode 1)))

;; Optional: Auto-open symbols view when entering LSP mode
;; Uncomment the following if you want symbols to always show
;; (add-hook 'lsp-mode-hook
;;           (lambda ()
;;             (when (lsp-feature? "textDocument/documentSymbol")
;;               (lsp-treemacs-symbols))))

;; Function to find and highlight the current symbol in the symbols view
(defun my/lsp-treemacs-symbols-highlight-current ()
  "Highlight the symbol at point in the LSP symbols view."
  (interactive)
  (when-let* ((symbols-buffer (get-buffer lsp-treemacs-symbols-buffer-name))
              (symbols-window (get-buffer-window symbols-buffer))
              (current-pos (point)))
    (with-current-buffer symbols-buffer
      (let ((inhibit-read-only t))
        (hl-line-mode 1)))))

;; Optional: Automatically highlight current symbol when cursor moves
;; This adds a small delay to avoid constant updates
;; Uncomment to enable:
;; (defvar my/lsp-symbols-highlight-timer nil)
;; (defun my/schedule-symbols-highlight ()
;;   (when my/lsp-symbols-highlight-timer
;;     (cancel-timer my/lsp-symbols-highlight-timer))
;;   (setq my/lsp-symbols-highlight-timer
;;         (run-with-idle-timer 0.5 nil #'my/lsp-treemacs-symbols-highlight-current)))
;;
;; (add-hook 'lsp-mode-hook
;;           (lambda ()
;;             (add-hook 'post-command-hook #'my/schedule-symbols-highlight nil t)))
