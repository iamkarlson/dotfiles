;;; ../src/dotfiles/doomemacs/org-roam.el -*- lexical-binding: t; -*-

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/braindb/")


(after! org
  (setq org-agenda-files (directory-files-recursively (file-truename org-directory) "\\.org$"))
  (setq warning-suppress-types (append warning-suppress-types '((org-element-cache))))
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; A project, which usually contains other tasks
           "LOOP(r)"  ; A recurring task
           "STRT(s)"  ; A task that is in progress
           "WAIT(w)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "IDEA(i)"  ; An unconfirmed and unapproved task or notion
           "|"
           "DONE(d!/!)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted, or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")  ; Task was completed
          (sequence
           "|"
           "OKAY(o)"
           "YES(y)"
           "NO(n)"))
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("NO"   . +org-todo-cancel)
          ("KILL" . +org-todo-cancel)))

  )


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
                              "#+title: ${title}\n")
           :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("c" "contact" plain (function my/org-roam-capture-contact)
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("i" "idea" plain
           ,(format "#+title: ${title}\n%%[%s/templates/idea.org.txt]" org-roam-directory)
           :target (file "thought/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ("t" "technology" plain "* %?"
           :if-new (file+head
                    "%<%Y%m%d%H%M%S>-${slug}.org"
                    ":PROPERTIES:\n:ROAM_ALIASES: ${alias}\n:END:\n#+title: ${title}\n\n\n* Characteristics\n- Documentation:\n- Developer:\n* Snippets:\n")
           :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          )
        org-roam-dailies-capture-templates
        '(("d" "default" entry "* %?"
           :target (file+head "%<%Y-%m>/%<%d - %A>.org" "%<%Y %B %d, %A, Week %V>\n\n* Goals for today\n** \n\n* Agenda \n- 10:00 API Daily Sync \n- \n\n* Open tickets in [[https://dexterenergy.atlassian.net/jira/software/projects/API/boards/2?assignee=712020%3A2d1033ce-f19e-42dc-b72e-bc70bc672df2][Jira]] \n- \n\n* Journal:"))))
  (setq-default visual-fill-column-mode t)
  )
(add-hook! org-mode-hook 'org-display-inline-images)



(defun my/org-roam-dailies-capture-today-and-maximize ()
  "Capture today's daily note in a maximized frame and close the frame when done."
  (interactive)
  (org-roam-dailies-capture-today)
  (delete-other-windows)
  (add-hook 'org-capture-after-finalize-hook 'delete-frame))

(defun my/delete-frame-if-no-other ()
  "Delete the current frame if it is the only one."
  (when (= (length (frame-list)) 1)
    (delete-frame)))

(add-hook 'org-capture-after-finalize-hook 'my/delete-frame-if-no-other)


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
