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




;; Define our custom shortmenu function
(defun my-custom-dashboard-widget-shortmenu ()
  "Custom dashboard shortmenu with three buttons:
1) Braindb Today's Note
2) Braindb Weekly Note
3) Open Dotfiles (Dired)."
  (insert "\n")
  ;; 1. Braindb Today's Note
  (insert-text-button
   "  Braindb Today's Note"
   'action (lambda (_)
             (projectile-switch-project-by-name "~/braindb/")
             (treemacs-add-and-display-current-project-exclusively)
             (org-roam-dailies-find-today))
   'face 'doom-dashboard-menu-title
   'follow-link t
   'help-echo "Switch to project 'braindb' and open today's Org-roam daily note")
  (insert "\n")

  ;; 2. Braindb Weekly Note
  (insert-text-button
   "  Braindb Weekly Note"
   'action (lambda (_)
             (projectile-switch-project-by-name "~/braindb/")
             (treemacs-add-and-display-current-project-exclusively)
             (let* ((ym     (format-time-string "%Y-%m"))
                    (wk     (format-time-string "%U"))
                    (fname  (expand-file-name
                             (format "%s/agenda-week-%s.org" ym wk)
                             org-roam-dailies-directory)))
               (unless (file-exists-p fname)
                 (org-roam-capture- :node      (org-roam-node-create)
                                    :templates org-roam-dailies-capture-templates
                                    :info      (list :file fname)))
               (find-file fname)))
   'face 'doom-dashboard-menu-title
   'follow-link t
   'help-echo "Switch to project 'braindb' and open this week's Org-roam weekly note")
  (insert "\n")

  ;; 3. Open Dotfiles (Dired)
  (insert-text-button
   "  Open Dotfiles (Dired)"
   'action (lambda (_)
             (projectile-switch-project-by-name "~/src/dotfiles/")
             (treemacs-add-and-display-current-project-exclusively )
             (let ((root (projectile-project-root)))
               (when root
                 (dired root))))
   'face 'doom-dashboard-menu-title
   'follow-link t
   'help-echo "Switch to project 'dotfiles' and open Dired at its root")

  ;; 4. Open agenda
  (insert-text-button
   "  Open Agenda"
   'action (lambda (_)
             (org-agenda-list))
   'face 'doom-dashboard-menu-title
   'follow-link t
   'help-echo "Open Org Agenda")
  (insert "\n\n")
  )

;; Replace the shortmenu function in the dashboard functions list
(setq +doom-dashboard-functions
      (append '(doom-dashboard-widget-banner)
              ;;'(my-custom-dashboard-widget-shortmenu)
              '(doom-dashboard-widget-shortmenu)
              '(doom-dashboard-widget-loaded)
              '(doom-dashboard-widget-footer)
              )
      )
