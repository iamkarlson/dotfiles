;;; ../src/dotfiles/doomemacs/parts/code.el -*- lexical-binding: t; -*-

;; In this file, we configure the code editing experience.
;; I would love to integrate taskfile.dev for task management instead of the makefile.

(defun my-go-task-menu ()
  "Run `go-task --list-all`, filter tasks, and display a menu to select a task."
  (interactive)
  (let* (
         ;;(default-directory (file-name-directory (or buffer-file-name default-directory)))
         (task-output (shell-command-to-string "go-task --list-all"))
         ;; Filter lines that start with '*'
         (task-lines (seq-filter (lambda (line) (string-match-p "^\\* " line))
                                 (split-string task-output "\n" t)))
         ;; Parse task names and descriptions
         (tasks (mapcar (lambda (line)
                          (when (string-match "^\\* \\([^:]+\\):\\(.*\\)" line)
                            (cons (string-trim (match-string 1 line)) ; Task name
                                  (string-trim (match-string 2 line))
                                  ))) ; Description
                        task-lines))
         ;; Create a completing-read menu
         (selected-task (completing-read "Select a task: "
                                         (mapcar (lambda (task)
                                                   (format "%s: %s" (car task) (cdr task)))
                                                 tasks)
                                         nil t)))
    ;; Print the selected task's name
    (message "Selected task: %s"  (car (split-string selected-task ":")) )
    (message "Tasks: %s" tasks)

    (let ((task-name (car (assoc (car (split-string selected-task ":")) tasks))))
      (message "Selected task: %s" task-name)
      ;; Use `compile` with the selected task
      (compile (format "go-task %s" task-name)))))


(map! :after evil
      :desc "Run a task from taskfile.dev"
      :leader
      :n "c c" #'my-go-task-menu
      )
