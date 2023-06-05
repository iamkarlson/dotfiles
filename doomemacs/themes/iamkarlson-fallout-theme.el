;;; iamkarlson-fallout-theme.el --- inspired by Fallout dark theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: June 5, 2023
;; Author: George Green <https://github.com/iamkarlson>
;; Maintainer: George Green <https://github.com/iamkarlson>
;;
;;; Commentary:
;;
;; IamKarlson own emacs theme
;;
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup iamkarlson-fallout-theme nil
  "Options for the theme."
  :group 'doom-themes)


;;
;;; Theme definition

(def-doom-theme iamkarlson-fallout
  "A dark theme inspired by fallout colors."

  ;; name        default   256           16
  (
   (bg        '("#1c1408" nil       nil            ))
   (bg-alt    '("#271a0c" nil       nil            ))
   (fg         '("#5b8512" "#5b8512" "brightwhite"  ))
   (fg-alt     '("#4f7410" "#4f7410" "white"        ))

   (base0      '("#1B2229" "black"   "black"        ))
   (base1      '("#1c1f24" "#1c1f24" "brightblack"  ))
   (base2      '("#202328" "#202328" "brightblack"  ))
   (base3      '("#23272e" "#23272e" "brightblack"  ))
   (base4      '("#3f444a" "#3f444a" "brightblack"  ))
   (base5      '("#95836f" "#95836f" "brightblack"  ))
   (base6      '("#73797e" "#73797e" "brightblack"  ))
   (base7      '("#9ca0a4" "#9ca0a4" "brightblack"  ))
   (base8      '("#dfdfdf" "#dfdfdf" "white"        ))

   (blue       '("#0075c4" "#0075c4" "brightblue"   ))
   (cyan       '("#898989" "#898989" "brightcyan"   ))
   (dark-blue  '("#0060a1" "#0060a1" "blue"         ))
   (dark-cyan  '("#4f7410" "#4f7410" "cyan"         ))
   (green      '("#7cb518" "#7cb518" "green"        ))
   (grey       '("#707a6a" "#707a6a" "brightblack"  ))
   (magenta    '("#d72638" "#d72638" "brightmagenta"))
   (orange     '("#ff7000" "#ff7000" "brightred"    ))
   (purple     '("#800080" "#800080" "purple"       ))
   (red        '("#ff4e00" "#ff4e00" "red"          ))
   (teal       '("#dbc077" "#dbc077" "brightgreen"  )) ;; more of a sand/beige color
   (violet     '("#76597b" "#76597b" "magenta"      ))
   (yellow     '("#ffbf00" "#ffbf00" "yellow"       ))

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      yellow)
   (vertical-bar   (doom-darken grey 0.4))
   (selection      (doom-darken dark-cyan 0.8))
   (builtin        yellow)
   (comments       grey)
   (doc-comments   grey)
   (constants      orange)
   (functions      purple)
   (keywords       red)
   (methods        (doom-darken purple 0.5))
   (operators      yellow)
   (type           green)
   (strings        (doom-darken teal 0.1))
   (variables      green)
   (numbers        teal)
   (region         (doom-darken dark-cyan 0.7))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; These are extra color variables used only in this theme; i.e. they aren't
   ;; mandatory for derived themes.
   (modeline-fg              fg)
   (modeline-fg-alt          base5)
   (modeline-bg              (doom-darken purple 0.1))
   (modeline-bg-alt          `(,(doom-darken (car bg-alt) 0.15) ,@(cdr bg)))
   (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg)))

   (-modeline-pad 1)

   ;; end of configuration and doom overrides
   )


  ;;;; Base theme face overrides
  (((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   ((font-lock-comment-face &override)
    :background (doom-lighten bg 0.05))
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground base8)

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background modeline-bg)
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt)))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Here's and below goes my customizations
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



   ;;;; org <built-in>
   (org-level-1 :foreground orange :bold t)
   (org-level-2 :foreground blue :bold t)
   (org-level-3 :foreground magenta :bold t)
   (org-level-4 :foreground violet :bold t)
   (org-level-5 :foreground red :bold t)
   (org-level-6 :foreground yellow :bold t)


)
     ;;;; Base theme variable overrides-
  ())

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (let ((base (file-name-directory load-file-name)))
    (add-to-list 'custom-theme-load-path base)))

(provide 'iamkarlson-fallout-theme)
;;; iamkarlson-fallout-theme.el ends here
