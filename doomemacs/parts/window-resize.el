;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Resize Mode
;;
;; Keyboard-driven window resizing using vim-style hjkl navigation.
;; Similar to Hyprland's resize submode but for Emacs windows.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my/window-resize-increment 5
  "Number of rows/columns to resize by in window-resize-mode.
Larger values make resize faster but less precise.
Recommended values: 2-10")

(defhydra my/hydra-window-resize (:color pink :hint nil)
  "
╔═══════════════════════════════════╗
║     Window Resize Mode (hjkl)     ║
╠═══════════════════════════════════╣
║  _h_ ←  shrink width              ║
║  _l_ →  enlarge width             ║
║  _j_ ↓  enlarge height            ║
║  _k_ ↑  shrink height             ║
║  _=_    balance windows           ║
║  _q_    quit (or any other key)   ║
╚═══════════════════════════════════╝
"
  ("h" (shrink-window-horizontally my/window-resize-increment))
  ("l" (enlarge-window-horizontally my/window-resize-increment))
  ("j" (enlarge-window my/window-resize-increment))
  ("k" (shrink-window my/window-resize-increment))
  ("=" balance-windows)
  ("q" nil :exit t))

(defun my/window-resize-mode ()
  "Enter window resize mode using vim-style hjkl navigation.

This creates a modal interface for resizing windows:
  h - shrink width (move right divider left)
  l - enlarge width (move right divider right)
  j - enlarge height (move bottom divider down)
  k - shrink height (move bottom divider up)
  = - balance windows to equal sizes
  q - quit explicitly (any other key also exits)

The resize increment is controlled by `my/window-resize-increment'.
This mode automatically exits when any non-resize key is pressed."
  (interactive)
  (if (one-window-p)
      (message "Only one window, nothing to resize")
    (my/hydra-window-resize/body)))
