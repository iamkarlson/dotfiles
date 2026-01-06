;; -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Georgy Green"
      user-mail-address "iamkarlson@gmail.com")



;; Set the default Hunspell program and dictionary path
(setq ispell-program-name "hunspell"
      ispell-really-hunspell t
      ispell-dictionary "en_US" ;; Default to English
      ispell-local-dictionary-alist
      '(("en_US"
         "[[:alpha:]]"
         "[^[:alpha:]]"
         "[']"
         nil
         ("-d" "en_US")
         nil
         utf-8)
        ("en_US,ru_RU,nl_NL"
         "[[:alpha:]]"
         "[^[:alpha:]]"
         "[']"
         nil
         ("-d" "en_US,ru_RU,nl_NL")
         nil
         utf-8)
        ("ru_RU"
         "[[:alpha:]]"
         "[^[:alpha:]]"
         "[']"
         nil
         ("-d" "ru_RU")
         nil
         utf-8)
        ("nl_NL"
         "[[:alpha:]]"
         "[^[:alpha:]]"
         "[']"
         nil
         ("-d" "nl_NL")
         nil
         utf-8)))

;; Configure hunspell dictionary paths
(setq ispell-hunspell-dict-paths-alist
      '(("en_US" "/usr/share/hunspell/en_US.dic")
        ("ru_RU" "/usr/share/hunspell/ru_RU.dic")
        ("nl_NL" "/usr/share/hunspell/nl_NL.dic")))

;; Set personal dictionary path
(setq ispell-personal-dictionary "~/.hunspell_personal")

;; Disable automatic deletion of trailing whitespace
(remove-hook 'before-save-hook 'delete-trailing-whitespace)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type `relative)


(+global-word-wrap-mode +1)
(after! visual-fill-column
  (setq-default visual-fill-column-width 200)
  (setq-default visual-fill-column-center-text t))
(setq-default doom-modeline-vcs-max-length 60)

(add-hook! 'magit-mode-hook
  (defun +my-markdown-mode-settings ()
    (evil-mode nil)
    (visual-line-mode 1)))


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
;; (setq-default evil-respect-visual-line-mode t) ;; Disabled - use actual lines for j/k


(menu-bar-mode +1)
(blink-cursor-mode +1)

;; Disabled by default
(global-wakatime-mode)

(setq-default org-insert-heading-respect-content t)
(setq-default treemacs-follow-after-init t)
(setq-default treemacs-project-follow-cleanup t)

;; Open dired when switching projects
;; But it seems to be broken in Doom Emacs currently
;;(setq-default projectile-switch-project-action 'projectile-dired)

;; Configure dired for case-insensitive sorting
(after! dired
  (setq ls-lisp-use-insert-directory-program nil)  ; Use Emacs' ls-lisp instead of system ls
  (require 'ls-lisp)
  (setq ls-lisp-ignore-case t))  ; Case-insensitive sorting

(defun my-evil-disable-remove-spaces ()
  "Disable automatic removal of trailing spaces in `evil-mode'."
  (setq-local evil-maybe-remove-spaces nil))



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

;; TRAMP configuration for remote file editing
(after! tramp
  (setq tramp-default-method "scp")
  (setq tramp-verbose 1) ; Set to 6 for debugging if issues occur
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;; Prevent TRAMP from hanging on shell output
  (setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*"))


(add-hook! 'gcode-mode-hook 'eldoc-mode)



(defun my/save-buffer-on-insert-exit ()
  "Save the current buffer when leaving insert mode."
  (when (buffer-file-name) (save-buffer)))
(add-hook! 'evil-insert-state-exit-hook #'my/save-buffer-on-insert-exit)


;; This little piece of shit was producing a lot of ~SPC <mouse-movement> is undefined~
;; Well, not anymore
(defun my/disable-mouse-hook()
  ;; Don't interfere with mouse operations on window borders
  (unless (or (minibufferp)
              (eq last-command 'mouse-drag-mode-line)
              (eq last-command 'mouse-drag-vertical-line)
              (eq this-command 'mouse-drag-mode-line)
              (eq this-command 'mouse-drag-vertical-line))
    (setq track-mouse nil)))

(defun my/refresh-visual-line-mode ()
  "Refresh visual-line-mode in all buffers where it's enabled to recalculate line breaks"
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when visual-line-mode
        (visual-line-mode -1)
        (visual-line-mode 1)))))

;; Don't disable mouse during window configuration changes (resizing)
;; (add-hook! 'window-configuration-change-hook #'my/disable-mouse-hook)
(add-hook! 'window-configuration-change-hook #'my/refresh-visual-line-mode)
(add-hook! 'prog-mode-hook #'my/disable-mouse-hook)
(add-hook! 'lsp-mode-hook #'my/disable-mouse-hook)
(add-hook! 'python-mode-hook #'my/disable-mouse-hook)
(add-hook! 'magit-mode-hook #'my/disable-mouse-hook)
(add-hook! 'python-ts-mode-hook #'my/disable-mouse-hook)

(setq-default track-mouse nil)

;; Enable proper mouse support for window operations
(setq mouse-autoselect-window nil)
(setq mouse-drag-copy-region nil)
(setq mouse-drag-and-drop-region nil)

;; Enable mouse tracking for drag operations
;;(setq track-mouse t)

;; Ensure window divider mouse operations work properly
(setq window-divider-default-right-width 2)
(setq window-divider-default-bottom-width 2)
(window-divider-mode 1)

;; Enable mouse resize for splits
(global-set-key [mode-line mouse-1] 'mouse-drag-mode-line)
(global-set-key [vertical-line mouse-1] 'mouse-drag-vertical-line)

(after! apheleia
  ;; Enable debug logging to see what's happening
  (setq apheleia-log-debug-info t)

  ;; Disable Apheleia's automatic indentation override so it doesn't add --tab-width
  (setq apheleia-formatters-respect-indent-level nil)

  ;; Disable apheleia for markdown files
  (setq apheleia-inhibit-functions
        (list (lambda () (derived-mode-p 'markdown-mode)))))

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
         (file-expand-wildcards (expand-file-name "parts/*.el" doom-user-dir)))
  (load! f))



;; Add the modules folder to the load path
;;(add-to-list 'load-path (expand-file-name "~/.doom.d/modules/" user-emacs-directory))

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook
  (prog-mode . copilot-mode)
  ;;(org-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("C-n" . 'copilot-next-completion)
              ("C-p" . 'copilot-previous-completion))

  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  ;;(add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(yaml-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2)))


(use-package! claude-code-ide
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools


(use-package! code-review
  :defer t
  :config
  (add-hook 'code-review-mode-hook #'emojify-mode)
  (setq code-review-fill-column 96)
  (setq code-review-gitlab-host "gitlab.com/api/v4")
  ;; (setq code-review-auth-login-marker 'forge)
  (define-key forge-topic-mode-map (kbd "C-c v \"") 'code-review-forge-pr-at-point)
  (define-key code-review-mode-map (kbd "C-c C-n") 'code-review-comment-jump-next)
  (define-key code-review-mode-map (kbd "C-c C-p") 'code-review-comment-jump-previous))



;; Custom function to find file in dotfiles project
(defun my/find-file-in-dotfiles ()
  "Find file in dotfiles project and switch to that project workspace."
  (interactive)
  (let* ((dotfiles-dir "~/src/dotfiles/")
         (expanded-dir (expand-file-name dotfiles-dir)))
    ;; Switch to the dotfiles project
    (projectile-switch-project-by-name expanded-dir)
    ;; Then find file in that project
    (projectile-find-file)))




;; Add local snippets directory
(after! yasnippet
  (add-to-list 'yas-snippet-dirs "~/.local/share/doom-snippets"))

(server-start)

;; (use-package! gptel
;;   :config
;;   (setq
;;    gptel-model 'gemini-pro
;;    gptel-backend (gptel-make-gemini "Gemini"
;; ;; Old key for testing
;;:key "AIzaSyC2mRdll5WWftIWEq2o_EmlURRq5fWfDxA"
;;:stream t)))
