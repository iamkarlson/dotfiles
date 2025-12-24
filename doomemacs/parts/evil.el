

(after! evil
  ;; Make horizontal movement cross lines
  (setq-default evil-cross-lines t)
  (setq-default evil-move-cursor-back nil)
  (setq-default evil-ex-search-vim-style-regexp nil)
  (setq-default evil-move-beyond-eol t)
  (setq-default evil-shift-width 2)
  (add-hook 'evil-insert-state-entry-hook #'my-evil-disable-remove-spaces))
