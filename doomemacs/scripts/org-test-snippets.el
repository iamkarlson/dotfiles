;;; org-test-snippets.el --- Interactive testing framework for org-mode styling -*- lexical-binding: t; -*-
;;
;; Author: Your Name
;; Created: 2026-01-06
;; Package-Requires: ((emacs "27.1") (org "9.0"))
;;
;;; Commentary:
;;
;; This file provides an interactive testing framework for org-mode visual
;; settings and face customizations. It allows you to quickly test different
;; configurations before committing them to your config.
;;
;;; Usage:
;;
;; 1. Open the test org file: ~/src/dotfiles/doomemacs/docs/org-formatting-example.org
;; 2. Split window (SPC w v or C-x 3)
;; 3. Open this file in the other window
;; 4. Place cursor after any form and press C-x C-e to evaluate
;; 5. Watch the test org buffer update in real-time!
;;
;; You can also use M-x commands:
;;   M-x org-test-minimal-setup
;;   M-x org-test-recommended-setup
;;   M-x org-test-full-setup
;;   M-x org-test-reset-all
;;   M-x org-test-show-settings
;;
;;; Code:

(require 'org)

;;; ----------------------------------------------------------------------------
;;; Configuration
;;; ----------------------------------------------------------------------------

(defvar org-test-buffer "org-formatting-example.org"
  "Name of the buffer to use for testing org-mode styling.
Set this to the buffer name of your test org file.")

(defvar org-test-default-line-spacing 0.1
  "Default line spacing value for testing (0.0-0.3 recommended).")

;;; ----------------------------------------------------------------------------
;;; Helper Functions
;;; ----------------------------------------------------------------------------

(defun org-test--get-buffer ()
  "Get the test buffer, or error if not found."
  (or (get-buffer org-test-buffer)
      (error "Test buffer '%s' not found. Please open the test org file first"
             org-test-buffer)))

(defun org-test--apply-to-buffer (fn &rest args)
  "Apply function FN with ARGS to the test buffer.
Returns the result of applying FN."
  (with-current-buffer (org-test--get-buffer)
    (apply fn args)))

(defun org-test--restart-org ()
  "Restart org-mode in the test buffer."
  (org-test--apply-to-buffer #'org-mode-restart))

(defun org-test--set-vars (&rest var-value-pairs)
  "Set variables in test buffer from VAR-VALUE-PAIRS.
Example: (org-test--set-vars 'org-pretty-entities t 'line-spacing 0.1)"
  (org-test--apply-to-buffer
   (lambda ()
     (let ((pairs var-value-pairs))
       (while pairs
         (let ((var (pop pairs))
               (val (pop pairs)))
           (if (memq var '(line-spacing))
               (setq-local (symbol-value var) val)
             (set var val))))))))

(defun org-test--set-face (face &rest attrs)
  "Set FACE attributes to ATTRS.
Example: (org-test--set-face 'org-document-info :height 1.15 :weight 'normal)"
  (apply #'set-face-attribute face nil attrs))

(defun org-test--reset-face (face)
  "Reset FACE to default attributes."
  (set-face-attribute face nil
                      :height 1.0
                      :weight 'normal
                      :foreground 'unspecified
                      :background 'unspecified))

;;; ----------------------------------------------------------------------------
;;; Individual Feature Testing Functions
;;; ----------------------------------------------------------------------------

(defun org-test-pretty-entities (&optional enable)
  "Toggle or set org-pretty-entities.
If ENABLE is non-nil, enable it. If nil, disable it.
If called interactively, toggle."
  (interactive)
  (let ((value (if (called-interactively-p 'any)
                   (not org-pretty-entities)
                 enable)))
    (setq org-pretty-entities value)
    (org-test--restart-org)
    (message "Pretty entities %s" (if value "enabled" "disabled"))))

(defun org-test-hide-emphasis (&optional enable)
  "Toggle or set org-hide-emphasis-markers.
If ENABLE is non-nil, enable it. If nil, disable it.
If called interactively, toggle."
  (interactive)
  (let ((value (if (called-interactively-p 'any)
                   (not org-hide-emphasis-markers)
                 enable)))
    (setq org-hide-emphasis-markers value)
    (org-test--restart-org)
    (message "Hide emphasis markers %s" (if value "enabled" "disabled"))))

(defun org-test-hide-macros (&optional enable)
  "Toggle or set org-hide-macro-markers.
If ENABLE is non-nil, enable it. If nil, disable it.
If called interactively, toggle."
  (interactive)
  (let ((value (if (called-interactively-p 'any)
                   (not org-hide-macro-markers)
                 enable)))
    (setq org-hide-macro-markers value)
    (org-test--restart-org)
    (message "Hide macro markers %s" (if value "enabled" "disabled"))))

(defun org-test-hide-stars (&optional enable)
  "Toggle or set org-hide-leading-stars.
If ENABLE is non-nil, enable it. If nil, disable it.
If called interactively, toggle."
  (interactive)
  (let ((value (if (called-interactively-p 'any)
                   (not org-hide-leading-stars)
                 enable)))
    (setq org-hide-leading-stars value)
    (org-test--restart-org)
    (message "Hide leading stars %s" (if value "enabled" "disabled"))))

(defun org-test-line-spacing (value)
  "Set line-spacing to VALUE in the test buffer.
Recommended values: 0.05 (subtle), 0.1 (moderate), 0.15 (spacious), 0.2 (extra).
Use nil to reset."
  (interactive "nLine spacing (0.05-0.2, or 0 to reset): ")
  (let ((spacing (if (= value 0) nil value)))
    (org-test--apply-to-buffer
     (lambda () (setq-local line-spacing spacing)))
    (message "Line spacing set to %s" (or spacing "default"))))

(defun org-test-indent-mode (&optional enable)
  "Toggle or set org-indent-mode.
If ENABLE is non-nil, enable it. If nil, disable it.
If called interactively, toggle."
  (interactive)
  (let ((value (if (called-interactively-p 'any)
                   (not (org-test--apply-to-buffer
                         (lambda () org-indent-mode)))
                 enable)))
    (org-test--apply-to-buffer
     (lambda () (org-indent-mode (if value 1 0))))
    (message "Org indent mode %s" (if value "enabled" "disabled"))))

;;; ----------------------------------------------------------------------------
;;; Face Testing Functions
;;; ----------------------------------------------------------------------------

(defun org-test-document-info-face (&optional height weight)
  "Set org-document-info face attributes.
HEIGHT defaults to 1.15, WEIGHT defaults to normal."
  (interactive)
  (let ((h (or height 1.15))
        (w (or weight 'normal)))
    (org-test--set-face 'org-document-info :height h :weight w)
    (message "Document info face: height %.2f, weight %s" h w)))

(defun org-test-document-keyword-face (&optional height foreground)
  "Set org-document-info-keyword face attributes.
HEIGHT defaults to 0.9, FOREGROUND defaults to #888888."
  (interactive)
  (let ((h (or height 0.9))
        (fg (or foreground "#888888")))
    (org-test--set-face 'org-document-info-keyword :height h :foreground fg)
    (message "Document keyword face: height %.2f, color %s" h fg)))

(defun org-test-reset-faces ()
  "Reset all org-mode faces to defaults."
  (interactive)
  (org-test--reset-face 'org-document-info)
  (org-test--reset-face 'org-document-info-keyword)
  (message "All test faces reset to defaults"))

;;; ----------------------------------------------------------------------------
;;; Preset Configurations
;;; ----------------------------------------------------------------------------

(defun org-test-minimal-setup ()
  "Apply minimal prettification setup (recommended starting point)."
  (interactive)
  (setq org-pretty-entities t
        org-hide-emphasis-markers t)
  (org-test--restart-org)
  (message "✓ Minimal setup applied: pretty entities + hidden emphasis"))

(defun org-test-recommended-setup ()
  "Apply recommended balanced setup."
  (interactive)
  (setq org-pretty-entities t
        org-hide-emphasis-markers t
        org-hide-macro-markers t
        org-hide-leading-stars t)
  (org-test--apply-to-buffer
   (lambda () (setq-local line-spacing org-test-default-line-spacing)))
  (org-test--restart-org)
  (org-test-document-info-face)
  (message "✓ Recommended setup applied: prettification + spacing + faces"))

(defun org-test-full-setup ()
  "Apply full setup with all enhancements including indentation."
  (interactive)
  (setq org-pretty-entities t
        org-hide-emphasis-markers t
        org-hide-macro-markers t
        org-hide-leading-stars t)
  (org-test--apply-to-buffer
   (lambda ()
     (setq-local line-spacing 0.15)
     (org-indent-mode 1)))
  (org-test--restart-org)
  (org-test-document-info-face)
  (org-test-document-keyword-face)
  (message "✓ Full setup applied: all enhancements + indentation"))

(defun org-test-reset-all ()
  "Reset all org-mode styling settings to defaults."
  (interactive)
  (setq org-pretty-entities nil
        org-hide-emphasis-markers nil
        org-hide-macro-markers nil
        org-hide-leading-stars nil)
  (org-test--apply-to-buffer
   (lambda ()
     (setq-local line-spacing nil)
     (org-indent-mode 0)))
  (org-test--restart-org)
  (org-test-reset-faces)
  (message "✓ All settings reset to defaults"))

;;; ----------------------------------------------------------------------------
;;; Inspection Functions
;;; ----------------------------------------------------------------------------

(defun org-test-show-settings ()
  "Display current org styling settings in minibuffer."
  (interactive)
  (let ((indent (org-test--apply-to-buffer
                 (lambda () org-indent-mode)))
        (spacing (org-test--apply-to-buffer
                  (lambda () line-spacing))))
    (message (concat "Org Test Settings:\n"
                     "  Pretty entities: %s\n"
                     "  Hide emphasis: %s\n"
                     "  Hide macros: %s\n"
                     "  Hide stars: %s\n"
                     "  Line spacing: %s\n"
                     "  Indent mode: %s")
             (if org-pretty-entities "✓" "✗")
             (if org-hide-emphasis-markers "✓" "✗")
             (if org-hide-macro-markers "✓" "✗")
             (if org-hide-leading-stars "✓" "✗")
             (or spacing "default")
             (if indent "✓" "✗"))))

;;; ----------------------------------------------------------------------------
;;; Quick Evaluation Snippets
;;; ----------------------------------------------------------------------------

;; These can be evaluated directly with C-x C-e for quick testing

;; Minimal setup (just prettification)
;; (org-test-minimal-setup)

;; Recommended setup (balanced)
;; (org-test-recommended-setup)

;; Full setup (everything enabled)
;; (org-test-full-setup)

;; Reset everything
;; (org-test-reset-all)

;; Show current settings
;; (org-test-show-settings)

;; Individual features
;; (org-test-pretty-entities t)
;; (org-test-hide-emphasis t)
;; (org-test-hide-macros t)
;; (org-test-hide-stars t)
;; (org-test-line-spacing 0.1)
;; (org-test-indent-mode t)

;; Face testing
;; (org-test-document-info-face 1.2 'bold)
;; (org-test-document-keyword-face 0.85 "#999999")
;; (org-test-reset-faces)

;;; ----------------------------------------------------------------------------
;;; Advanced: Direct API for Custom Testing
;;; ----------------------------------------------------------------------------

;; For advanced users who want to test custom combinations:

;; Apply custom variable combination
;; (progn
;;   (setq org-pretty-entities t
;;         org-hide-emphasis-markers t)
;;   (org-test--apply-to-buffer
;;    (lambda () (setq-local line-spacing 0.12)))
;;   (org-test--restart-org))

;; Test custom face attributes
;; (org-test--set-face 'org-level-1 :height 1.4 :weight 'extra-bold)

;;; ----------------------------------------------------------------------------
;;; Quick Reference
;;; ----------------------------------------------------------------------------

;; Interactive Commands:
;;   M-x org-test-minimal-setup
;;   M-x org-test-recommended-setup
;;   M-x org-test-full-setup
;;   M-x org-test-reset-all
;;   M-x org-test-show-settings
;;
;; Individual Toggles:
;;   M-x org-test-pretty-entities
;;   M-x org-test-hide-emphasis
;;   M-x org-test-hide-macros
;;   M-x org-test-hide-stars
;;   M-x org-test-indent-mode
;;
;; Adjustments:
;;   M-x org-test-line-spacing
;;   M-x org-test-document-info-face
;;
;; Quick eval (C-x C-e after each line):
;;   (org-test-recommended-setup)
;;   (org-test-line-spacing 0.15)
;;   (org-test-show-settings)
;;
;; M-: one-liners (for testing outside this file):
;;   M-: (setq org-pretty-entities t)
;;   M-: (org-mode-restart)
;;   M-: (setq-local line-spacing 0.1)

(provide 'org-test-snippets)
;;; org-test-snippets.el ends here
