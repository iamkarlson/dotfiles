;; -*- lexical-binding: t; -*-

(setq fancy-splash-image (concat doom-user-dir "splash.jpg"))


;; ---- helpers -------------------------------------------------------------

(defun +my/find-project-path (name)
  "Return the *absolute path* of the Projectile project named NAME.
NAME is compared with the last directory of each project path."
  (or (seq-find (lambda (p)
                  (string-equal name
                                (file-name-nondirectory
                                 (directory-file-name p)
                                 )
                                )
                  )
                projectile-known-projects)
      (user-error "Project “%s” not in projectile-known-projects" name)
      )
  )

(defun +my/goto-project! (name)
  "Switch Treemacs and Projectile to project NAME."
  (let (
        (path (+my/find-project-path name)
              )
        )
    (treemacs-add-and-display-current-project-exclusively)
    (projectile-switch-project-by-name path)
    )
  )

(defun +my/org-roam-weekly ()
  "Open or create this week’s Org-roam weekly note."
  (interactive)
  (let* (
         (ym  (format-time-string "%Y-%m"))
         (wk  (format-time-string "%U"))
         (fn  (expand-file-name
               (format "%s/agenda-week-%s.org" ym wk)
               org-roam-directory)
              )
         )
    (if (file-exists-p fn)
        (find-file fn)
      (org-roam-capture-
       :node (org-roam-node-create)
       :info (list :file fn)
       :templates org-roam-dailies-capture-templates)
      )
    )
  )

;; ---- dashboard override --------------------------------------------------

(after! doom-dashboard
  ;; *Replace* the menu, don’t append
  (setq! +doom-dashboard-menu-sections
         '(("Braindb → Today"
            :action (lambda! (+my/goto-project! "braindb")
                      (org-roam-dailies-find-today)))
           ("Braindb → Weekly"
            :action (lambda! (+my/goto-project! "braindb")
                      (+my/org-roam-weekly)))
           ("Dotfiles (Dired)"
            :action (lambda! (+my/goto-project! "dotfiles")
                      (dired (projectile-project-root))))))

  ;; Don’t rely on Doom’s default widget (we nuke & re-add)
  (remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
  (add-hook    '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
  )
