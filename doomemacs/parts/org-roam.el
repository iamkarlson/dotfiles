;;; ../src/dotfiles/doomemacs/org-roam.el -*- lexical-binding: t; -*-

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/braindb/")
(setq org-roam-directory "~/braindb/")



(after! org-roam
  (setq org-roam-capture-templates
        `(
          ("d" "default" plain
           "* %?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("i" "idea" plain
           ,(format "#+title: ${title}\n%%[%s/templates/idea.org.txt]" org-roam-directory)
           :target (file "thought/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("t" "technology" plain "* %?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" ":PROPERTIES:\n:ROAM_ALIASES: ${alias}\n:END:\n#+title: ${title}\n\n\n* Characteristics\n- Documentation:\n- Developer:\n* Snippets:\n")
           :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          )
        org-roam-dailies-capture-templates
        '(("d" "default" entry "* %?"
          :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%B %d, %Y>\n\n* Goals for today\n- \n\n* Agenda \n- \n\n* Open tickets in [[https://gitlab.com/groups/dexter-energy/-/boards/2728779?assignee_username=georgydexter][gitlab]] \n- \n\n* Notes")))))



(map! :after evil
      :desc "Roam Capture"
      :leader
      :n "X" #'org-roam-dailies-capture-today
      :desc "Roam Today"
      :leader
      :n "j" #'org-roam-dailies-goto-today
      )



;; Pomodoro
(setq-default org-pomodoro-length 15
              org-pomodoro-short-break-length 3)
