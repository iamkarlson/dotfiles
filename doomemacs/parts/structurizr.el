;;; ../src/dotfiles/doomemacs/parts/structurizr.el -*- lexical-binding: t; -*-

;; Use structurizr-mode for "workspace.dsl" files
(add-to-list 'auto-mode-alist '("workspace\\.dsl\\'" . structurizr-mode))

;; Hook for configuring structurizr-mode
(add-hook 'structurizr-mode-hook
          (lambda ()
            ;; Enable line numbers
            (display-line-numbers-mode 1)
            ;; Add any additional configurations here
            ))
