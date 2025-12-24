;; -*- lexical-binding: t; -*-


(map! :map dired-mode-map
      :n "c" #'dired-create-empty-file)

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
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line))


(map! :after evil
      :leader

      :desc "Toggle visual fill column mode"
      :n "t z" #'visual-fill-column-mode

      :desc "Toggle visual line mode"
      :n "t x" #'visual-line-mode

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
