;; -*- lexical-binding: t; -*-


(map! :map dired-mode-map
      :n "c" #'dired-create-empty-file)

(defun my/evil-use-visual-line-movement ()
  "Make j/k move by visual lines (wrapped lines) instead of actual lines.
This is useful for prose editing where long lines wrap."
  (interactive)
  (setq evil-respect-visual-line-mode t)
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (message "Evil movement: using visual lines (j/k move by wrapped lines)"))

(defun my/evil-use-actual-line-movement ()
  "Make j/k move by actual lines (standard Vim behavior).
gj/gk will move by visual lines instead."
  (interactive)
  (setq evil-respect-visual-line-mode nil)
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") nil)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") nil)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") nil)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") nil)
  (message "Evil movement: using actual lines (j/k move by real lines, gj/gk by wrapped)"))

(after! evil
  ;; Horizontal movement (unchanged)
  (define-key evil-normal-state-map (kbd "<remap> <evil-backward-char>") 'left-char)
  (define-key evil-motion-state-map (kbd "<remap> <evil-forward-char>") 'right-char)
  (define-key evil-normal-state-map (kbd "<remap> <evil-backward-char>") 'left-char)
  (define-key evil-motion-state-map (kbd "<remap> <evil-forward-char>") 'right-char)

  ;; Use standard Vim behavior by default (j/k = actual lines, gj/gk = visual lines)
  (my/evil-use-actual-line-movement))


(map! :after evil
      :leader

      :desc "Toggle visual fill column mode"
      :n "t z" #'visual-fill-column-mode

      :desc "Toggle visual line mode"
      :n "t x" #'visual-line-mode

      :desc "Use visual line movement (j/k for wrapped lines)"
      :n "t v" #'my/evil-use-visual-line-movement

      :desc "Use actual line movement (j/k for real lines)"
      :n "t V" #'my/evil-use-actual-line-movement

      :desc "Toggle links display"
      :n "t h" #'org-toggle-link-display

      :desc "Toggle images display"
      :n "t p" #'org-toggle-inline-images

      :desc "Toggle evil mode"
      :n "t e" #'evil-mode

      :desc "Show project in treemacs"
      :n "p l" #'treemacs-add-and-display-current-project-exclusively

      :desc "Repeat last substitute"
      :n "&" #'evil-ex-repeat-substitute

      :desc "Copy buffer name"
      :n "b Y" (lambda () (interactive) (kill-new (buffer-name)))

      ;; Project file navigation overrides
      :desc "Find file in current project"
      :n "f p" #'projectile-find-file

      :desc "Find file in dotfiles project"
      :n "p f" #'my/find-file-in-dotfiles

      :desc "Copy project name"
      :n "p y" (lambda () (interactive) (kill-new (projectile-project-name)))

      :desc "Copy project path"
      :n "p Y" (lambda () (interactive) (kill-new (projectile-project-root)))
      )

(map! :after evil
      :leader
      
      :desc "Roam Capture"
      :n "X" #'org-roam-dailies-capture-today
      
      :desc "Roam Today"
      :n "d" #'org-roam-dailies-goto-today


      :desc "Open Journal Node"
      :n "j" (lambda ()
               (interactive)
               (let (
                     (node (org-roam-node-from-title-or-alias "Log journal"))
                     )
                 (if node
                     (org-roam-node-visit node)
                   (message "journal.org node not found."))))

      )

(map! :after org
      :map org-mode-map
      :desc "Insert inactive timestamp"
      "C-c ." #'org-timestamp-inactive)
