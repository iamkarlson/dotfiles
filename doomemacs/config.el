;; -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Georgy Green"
      user-mail-address "iamkarlson@gmail.com")


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

;; Set the default Hunspell program and dictionary path
(setq ispell-program-name "hunspell"
      ispell-really-hunspell t
      ispell-dictionary "en_US,ru_RU" ;; Default dictionaries to use
      ispell-local-dictionary-alist
      '(("en_US,ru_RU"
         "[[:alpha:]]"
         "[^[:alpha:]]"
         "[']"
         nil
         ("-d" "en_US,ru_RU")
         nil
         utf-8)))

;; Disable automatic deletion of trailing whitespace
(remove-hook 'before-save-hook 'delete-trailing-whitespace)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type `relative)


(+global-word-wrap-mode +1)
(setq-default visual-fill-column-width 200)
(setq-default visual-fill-column-center-text t)
(setq-default doom-modeline-vcs-max-length 60)

(add-hook! 'magit-mode-hook
  (defun +my-markdown-mode-settings ()
    (evil-mode nil)
    (visual-line-mode 1)
    )
  )


;;(setq-default visual-line-mode t)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;(remove-hook 'doom-first-input-hook #'evil-snipe-mode)
(setq-default evil-kill-on-visual-paste nil)
(setq-default evil-respect-visual-line-mode t)


(menu-bar-mode +1)
(blink-cursor-mode +1)

;; Disabled by default
(global-wakatime-mode)

(setq-default org-insert-heading-respect-content t)
(setq-default treemacs-follow-after-init t)
(setq-default treemacs-project-follow-cleanup t)



(defun my-evil-disable-remove-spaces ()
  "Disable automatic removal of trailing spaces in `evil-mode'."
  (setq-local evil-maybe-remove-spaces nil))


(after! evil
  ;;
  ;; kill two birds with one stone using remap: arrow keys and h,j,k,l
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-backward-char>") 'left-char)
  (define-key evil-motion-state-map (kbd "<remap> <evil-forward-char>") 'right-char)
  (define-key evil-normal-state-map (kbd "<remap> <evil-backward-char>") 'left-char)
  (define-key evil-motion-state-map (kbd "<remap> <evil-forward-char>") 'right-char)

  ;; Make movement keys work like they should
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  ;; Make horizontal movement cross lines
  (setq-default evil-cross-lines t)
  (setq-default evil-move-cursor-back nil)
  (setq-default evil-ex-search-vim-style-regexp nil)
  (setq-default evil-move-beyond-eol t)
  (setq-default evil-shift-width 2)
  (add-hook 'evil-insert-state-entry-hook #'my-evil-disable-remove-spaces)
  )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(setq-default tab-width 2)            ; Set width for tabs
(setq-default standard-indent 2)      ; Set default indentation
(setq-default indent-tabs-mode nil)   ; Use spaces instead of tabs

(setq-default visual-line-mode t)



(setq projectile-project-search-path '(
                                       ("~/src/" . 2)
                                       ("~/src/work/" . 2)
                                       ("~/activities/". 2)
                                       )
      )

(setq password-cache-expiry nil)


(add-hook! 'gcode-mode-hook 'eldoc-mode)



(defun my/save-buffer-on-insert-exit ()
  "Save the current buffer when leaving insert mode."
  (when (buffer-file-name) (save-buffer)))
(add-hook! 'evil-insert-state-exit-hook #'my/save-buffer-on-insert-exit)


;; This little piece of shit was producing a lot of ~SPC <mouse-movement> is undefined~
;; Well, not anymore
(defun my/disable-mouse-hook()
  (setq track-mouse nil))

(add-hook! 'window-configuration-change-hook #'my/disable-mouse-hook)
(add-hook! 'prog-mode-hook #'my/disable-mouse-hook)
(add-hook! 'lsp-mode-hook #'my/disable-mouse-hook)
(add-hook! 'python-mode-hook #'my/disable-mouse-hook)
(add-hook! 'magit-mode-hook #'my/disable-mouse-hook)


(setq case-fold-search t)   ; make searches case insensitive
(setq completion-ignore-case t) ; make autocomplete case insensitive

(after! treemacs
  (setq treemacs-follow-after-init t)
  (setq treemacs-project-follow-cleanup t)
  (setq treemacs-read-string-input 'from-minibuffer))

(after! lsp-python-ms
  (setq dap-python-debugger 'debugpy))

(use-package! treemacs
  :init
  (setq treemacs-follow-after-init t)
  (setq      treemacs-is-never-other-window t)
  (setq  treemacs-project-follow-cleanup t)
  (setq treemacs-collapse-dirs 3)
  (setq     treemacs-width 40)
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'simple)
  (treemacs-fringe-indicator-mode t))

(after! ediff
  (setq ediff-scroll-vertically t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This block here is very important because
;; it loads stuff from the ~parts/~ folder
;; You may have some UB if you just add a file there and
;; it's gonna be loaded automatically
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dolist (f
         (file-expand-wildcards (expand-file-name "parts/*.el" doom-user-dir))
         )
  (load! f)
  )



;; Add the modules folder to the load path
;;(add-to-list 'load-path (expand-file-name "~/.doom.d/modules/" user-emacs-directory))

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook
  (prog-mode . copilot-mode)
  (org-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("C-n" . 'copilot-next-completion)
              ("C-p" . 'copilot-previous-completion))

  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(yaml-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2)))



                                        ;(global-copilot-mode 1)


(map! :after evil
      :desc "Toggle visual line mode"
      :leader
      :n "t z" #'visual-fill-column-mode
      :desc "Toggle visual line mode"
      :leader
      :n "t x" #'visual-line-mode
      :desc "Toggle evil mode"
      :leader
      :n "t e" #'evil-mode
      :leader
      :n "p l" #'treemacs-add-and-display-current-project-exclusively
      :n "&" #'evil-ex-repeat-substitute
      :desc "Repeat last substitute"
      )


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

(server-start)

;; (use-package! gptel
;;   :config
;;   (setq
;;    gptel-model 'gemini-pro
;;    gptel-backend (gptel-make-gemini "Gemini"
;; ;; Old key for testing
;;:key "AIzaSyC2mRdll5WWftIWEq2o_EmlURRq5fWfDxA"
;;:stream t)))

