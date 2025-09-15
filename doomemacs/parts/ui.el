;; -*- lexical-binding: t; -*-

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;; (setq doom-font (font-spec :family "IntelOne Mono" :size 15 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "IntelOne Mono" :size 16))
(setq doom-font (font-spec :family "Hack Nerd Font" :size 15 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Hack Nerd Font" :size 16))

;;(setq doom-symbol-font doom-font)
;; TEST: 0000000      e5ca 
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;;      wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;;
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-tomorrow-night)
;;(setq doom-theme 'doom-manegarm)
;;(setq doom-theme 'iamkarlson-fallout)
;; (setq doom-theme 'doom-henna
;;       doom-henna-brighter-comments t)

;;(setq doom-theme 'doom-feather-light)
;; (setq doom-theme 'ef-autumn)
;; (setq doom-theme 'iamkarlson-fallout)
(setq doom-theme 'doom-badger)

(after! doom-themes
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (setq custom-safe-themes t))
                                        ;(load-theme 'iamkarlson-fallout t)

(setq sml/no-confirm-load-theme t)

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(after! doom-dashboard
  (defun +doom-dashboard-widget-shortmenu ()
    "Show only three dashboard buttons:
1) Braindb Today’s Note
2) Braindb Weekly Note
3) Open Dotfiles (Dired)."
    (let ((width +doom-dashboard--width))
      ;; Add a blank line for spacing
      (insert "\n")
      ;; 1. Braindb Today’s Note
      (widget-create
       'item
       ;; Display text
       :tag   (format "  %-32s" "Braindb Today’s Note")
       ;; Icon (optional; you can drop :icon entirely if you don't want an icon)
       :button-face 'doom-dashboard-menu-title
       :help-echo  "Switch to project 'braindb' and open today's Org-roam daily note"
       :action
       (lambda (&rest _)
         ;; 1a. Switch Treemacs to braindb
         (treemacs-add-and-display-current-project-exclusively "braindb")
         ;; 1b. Open today's Org-roam daily note
         (org-roam-dailies-find-today)))
      (insert (propertize (format " %s\n"
                                  ;; Show the keybinding help hint (e.g. [ d t ])
                                  (substitute-command-keys "\\[org-roam-dailies-find-today]"))
                          'face 'doom-dashboard-menu-desc))
      ;; 2. Braindb Weekly Note
      (widget-create
       'item
       :tag   (format "  %-32s" "Braindb Weekly Note")
       :button-face 'doom-dashboard-menu-title
       :help-echo  "Switch to project 'braindb' and open this week's Org-roam weekly note"
       :action
       (lambda (&rest _)
         ;; 2a. Switch Treemacs to braindb
         (treemacs-add-and-display-current-project-exclusively "braindb")
         ;; 2b. Compute year-month and week number
         (let* ((ym     (format-time-string "%Y-%m"))
                (wk     (format-time-string "%U"))
                (fname  (expand-file-name
                         (format "%s/agenda-week-%s.org" ym wk)
                         org-roam-dailies-directory)))
           ;; If the weekly file doesn't exist, create it via `org-roam-capture-`
           (unless (file-exists-p fname)
             (org-roam-capture- :node      (org-roam-node-create)
                                :templates org-roam-dailies-capture-templates
                                :info      (list :file fname)))
           ;; Finally, visit it
           (find-file fname))))
      (insert (propertize (format " %s\n"
                                  (substitute-command-keys "\\[org-roam-capture-]"))
                          'face 'doom-dashboard-menu-desc))
      ;; 3. Open Dotfiles (Dired)
      (widget-create
       'item
       :tag   (format "  %-32s" "Open Dotfiles (Dired)")
       :button-face 'doom-dashboard-menu-title
       :help-echo  "Switch to project 'dotfiles' and open Dired at its root"
       :action
       (lambda (&rest _)
         ;; 3a. Switch Treemacs to dotfiles
         (treemacs-add-and-display-current-project-exclusively "dotfiles")
         ;; 3b. Open Dired at that project's root
         (let ((root (projectile-project-root)))
           (when root
             (dired root)))))
      (insert (propertize (format " %s\n"
                                  (substitute-command-keys "\\[dired]"))
                          'face 'doom-dashboard-menu-desc))
      ;; Add a final newline for padding
      (insert "\n")))
  ;; Replace the default shortmenu hook with our custom one
  (remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
  (add-hook    '+doom-dashboard-functions #'+doom-dashboard-widget-shortmenu))
