;;; ../src/dotfiles/doomemacs/parts/code.el -*- lexical-binding: t; -*-

;; In this file, we configure the code editing experience.
;; I would love to integrate taskfile.dev for task management instead of the makefile.

(setq my-project-task-command "go-task --list-all")
(setq my-global-task-command "go-task -g --list-all")

(defun my-select-and-run-task (task-command)
  "Generic function to select and run a task using TASK-COMMAND."
  (interactive)
  (let* ((default-directory (file-name-directory (or buffer-file-name default-directory)))
         (task-output (shell-command-to-string task-command))
         ;; Filter lines starting with '*'
         (task-lines (seq-filter (lambda (line)
                                   (string-match-p "^\\* " line))
                                 (split-string task-output "\n" t)))
         ;; Parse task names and descriptions
         (tasks (mapcar (lambda (line)
                          (when (string-match "^\\* \\([^:]+\\):\\(.*\\)" line)
                            (cons (string-trim (match-string 1 line))  ;; Task name
                                  (string-trim (match-string 2 line))))) ;; Description
                        task-lines))
         ;; Create a completing-read menu
         (selected-task (completing-read "Select a task: "
                                         (mapcar (lambda (task)
                                                   (format "%s: %s" (car task) (cdr task)))
                                                 tasks)
                                         nil t)))
    ;; Extract the task name from the selected menu item
    (let ((task-name (car (assoc (car (split-string selected-task ":")) tasks))))
      ;; Use `comint` to run the command interactively
      (let ((compile-command (format "go-task %s" task-name)))
        (compilation-start
         compile-command
         'comint-mode
         (lambda (_) "*Go Task Output*"))
        ;; Add password handling once the buffer is ready
        (add-hook 'compilation-start-hook
                  (lambda ()
                    (message "Adding password prompt")
                    (when (string= (buffer-name) "*Go Task Output*")
                      (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt nil t)))
                  nil t)))))

(defun my-close-compilation-window (buffer status)
  "Close the compilation window if the task completes successfully."
  (when (and (string-match "finished" status)
             (string-match "\\*Go Task Output\\*" (buffer-name buffer))) ;; Match buffer name as a string

    (message "Buffer: %s"  (buffer-name buffer))
    (message "Task completed with status lalalal")
    (delete-window (get-buffer-window buffer)))
  (message "Task completed with status: %s" status))

(add-hook 'compilation-finish-functions #'my-close-compilation-window)


(defun my-project-tasks ()
  "Run project-local tasks."
  (interactive)
  (my-select-and-run-task my-project-task-command))

(defun my-global-tasks ()
  "Run global tasks."
  (interactive)
  (my-select-and-run-task my-global-task-command))



(map! :after evil
      :leader
      :desc "Run Project Task"
      :n "c b" #'my-project-tasks
      :desc "Run Global Task"
      :n "c g" #'my-global-tasks)
