;;; ../src/dotfiles/doomemacs/org-roam.el -*- lexical-binding: t; -*-

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/braindb/")


(after! org
  (setq org-agenda-files (directory-files-recursively (file-truename org-directory) "\\.org$")))


(setq org-roam-directory "~/braindb/")


(defun my/org-roam-capture-contact ()
  (let ((is-colleague (y-or-n-p "Is this a current colleague? ")))
    (concat
     (if is-colleague
         "* My colleague from [[roam:Dexter Energy]]\n"
       "")
     "* %?")))

(after! org-roam
  (setq org-roam-capture-templates
        `(
          ("d" "default" plain
           "* %?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "${title}\n")
           :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("c" "contact" plain (function my/org-roam-capture-contact)
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "${title}\n")
           :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("i" "idea" plain
           ,(format "${title}\n%%[%s/templates/idea.org.txt]" org-roam-directory)
           :target (file "thought/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("t" "technology" plain "* %?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" ":PROPERTIES:\n:ROAM_ALIASES: ${alias}\n:END:\n${title}\n\n\n* Characteristics\n- Documentation:\n- Developer:\n* Snippets:\n")
           :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          )
        org-roam-dailies-capture-templates
        '(("d" "default" entry "* %?"
           :target (file+head "%<%Y-%m>/%<%d - %A>.org" "%<%Y %B %d, %A, Week %V>\n\n* Goals for today\n** \n\n* Agenda \n- 10:00 API Daily Sync \n- \n\n* Open tickets in [[https://dexterenergy.atlassian.net/jira/software/projects/API/boards/2?assignee=712020%3A2d1033ce-f19e-42dc-b72e-bc70bc672df2][Jira]] \n- \n\n* Journal:")))))



(map! :after evil
      :desc "Roam Capture"
      :leader
      :n "X" #'org-roam-dailies-capture-today
      :desc "Roam Today"
      :leader
      :n "d" #'org-roam-dailies-goto-today


      :desc "Open Journal Node"
      :leader
      :n "j" (lambda () (interactive)
               (let ((node (org-roam-node-from-title-or-alias "Log journal")))
                 (if node
                     (org-roam-node-visit node)
                   (message "journal.org node not found."))))

      )



;; Pomodoro
(setq-default org-pomodoro-length 15
              org-pomodoro-short-break-length 3)


(defun org-roam-dailies-calendar-preview ()
  (let ((date (calendar-cursor-to-date t)))
    (when date
      (org-roam-dailies--capture date t)
      (other-window 1))))

(defun setup-org-roam-dailies-calendar-preview ()
  (add-hook 'calendar-move-hook 'org-roam-dailies-calendar-preview))

(with-eval-after-load 'calendar
  (setup-org-roam-dailies-calendar-preview))
