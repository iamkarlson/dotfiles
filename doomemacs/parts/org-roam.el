;;; ../src/dotfiles/doomemacs/org-roam.el -*- lexical-binding: t; -*-

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/braindb/")
(setq org-roam-directory "~/braindb/")


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



(add-hook! 'org-mode-hook
  (defun +my-org-mode-settings ()
    (git-auto-commit-mode 1)
    (visual-fill-column-mode 1)
    (visual-line-mode 1)
    ;;(print "hook added")
    ))

(add-hook! 'org-agenda-mode-hook
  (defun +my-org-agenda-settings ()
    (visual-fill-column-mode -1) ;; Turn off visual-fill-column-mode
    (visual-line-mode -1)        ;; Turn off visual-line-mode (word-wrap)
    (evil-local-mode -1)))       ;; Turn off evil-mode locally in the buffer


(defun my-auto-commit-message (filename)
  "Specify that my commit is a work in progress"
  (concat "braindb connect from " (system-name) ". file: " (gac-relative-file-name filename)))

(with-eval-after-load 'git-auto-commit-mode
  (setq gac-default-message #'my-auto-commit-message
                                        ;gac-ask-for-summary-p t
        ;; gac-automatically-push-p t
        gac-debounce-interval 30)
  )


;; (add-hook! 'org-mode-hook
;;   (lambda ()
;;     (setq visual-fill-column-width 100
;;           visual-fill-column-center-text t)
;;     (copilot-mode 1)
;;     (visual-fill-column-mode 1)))

;; this is mostly an example how to use a function as a callback for a capture template
(defun my/org-roam-capture-contact ()
  (let ((is-colleague (y-or-n-p "Is this a current colleague? ")))
    (concat
     (if is-colleague
         "* My colleague from [[roam:Dexter Energy]]\n"
       "")
     "* %?")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This is the most horrible part of org-roam. I absotely hate it.
;; ~I want to place my templates in a private repo, but I can't outsource the templates from config~
;;
;; Update: it seems like I found the way eventually, but haven't updated the comment.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/read-file (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(after! org-roam
  (setq org-roam-capture-templates
        `(
          ;; Nothing special
          ("d" "default" plain
           "* %?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ;; Contact
          ("c" "contact" plain (function my/org-roam-capture-contact)
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ;; Idea
          ("i" "idea" plain
           ,(my/read-file (concat org-roam-directory "templates/idea.org"))
           :target (file "thought/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ;; Technology or tool
          ("t" "technology" plain "* %?"
           :if-new (file+head
                    "%<%Y%m%d%H%M%S>-${slug}.org"
                    ,(my/read-file (concat org-roam-directory "templates/technology.org"))
                    )
           :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)
          ;; Area
          ("a" "area" plain "* %?"
           :if-new (file+head
                    "area/%<%Y%m%d%H%M%S>-${slug}.org"
                    ,(my/read-file (concat org-roam-directory "templates/area.org")))
           :target (file "area/%<%Y%m%d%H%M%S>-${slug}.org")
           :unnarrowed t)

          )
        ;; This is daily template
        org-roam-dailies-capture-templates
        `(("d" "default" entry "* %?"
           :target (file+head "%<%Y-%m>/%<%d - %A>.org" ,(my/read-file (concat org-roam-directory "templates/daily.org"))))

          ("w" "weekly" entry "* %?"
           :target (file+head "%<%Y-%m>/agenda-week-%<%U>.org" ,(my/read-file (concat org-roam-directory "templates/weekly.org"))))
          ))
  (setq-default visual-fill-column-mode t))
(add-hook! org-mode-hook 'org-display-inline-images)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This supposed to be called from a desktop file in hyprland
;; so I can make an action on the keyboard to call it on
;; and quickly type something not to forget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
      :n "j" (lambda ()
               (interactive)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EXPORT SETTINGS
;;
;; All about pdf/markdown/html for default org export
;; But also about exporting agenda data
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq org-latex-compiler "xelatex")

(after! org
  (setq org-latex-packages-alist
        '(("" "fontspec" t)        ; Load fontspec for xelatex/lualatex
          ("" "polyglossia" t)))   ; Load polyglossia for multilingual support
  (setq org-latex-default-packages-alist
        '(("" "amssymb" t))) ; Add the amssymb package for symbols
  ;; Disable Table of Contents for LaTeX exports
  ;;(setq org-latex-with-toc nil)
  (setq org-latex-toc-command nil)
  (setq org-latex-classes
        '(("article"
           "\\documentclass[14pt]{article}
            \\usepackage[margin=1in]{geometry} % Set minimal margins
                \\usepackage{fontspec}
                \\usepackage[russian]{babel}
                \\usepackage{polyglossia}
                \\setmainlanguage{english}
                \\setotherlanguage{russian}
                \\defaultfontfeatures{mapping=tex-text,scale=matchlowercase}

                \\setmainfont{DejaVu Serif}
                \\setmonofont{Courier New}
                \\usepackage{hyperref}
                \\hypersetup{pdfauthor={},pdftitle={},pdfsubject={},pdfkeywords={},pdfproducer={},pdfcreator={}}

                \\usepackage{titlesec}
                \\newcommand{\\sectionbreak}{\\clearpage}
"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))
;;
;; Add this to your config
(defun my/org-set-project-root-default-directory ()
  "Set `default-directory` to project root if in org-mode."
  (when (derived-mode-p 'org-mode)
    (let ((root (or (projectile-project-root) (vc-root-dir))))
      (when root
        (setq-local default-directory root)))))

(add-hook! 'org-mode-hook #'my/org-set-project-root-default-directory)
