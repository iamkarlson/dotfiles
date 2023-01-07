// .config/oni/config.tsx

import * as React from "react"
import * as Oni from "oni-api"

export const activate = (oni: Oni.Plugin.Api) => {
  console.log("config activated")

  oni.input.unbind("<c-g>") // make C-g work as expected in vim
  oni.input.unbind("<c-v>") // make C-v work as expected in vim
  oni.input.unbind("<c-t>") // make C-v work as expected in vim
  oni.input.bind("<s-c-g>", () => oni.commands.executeCommand("sneak.show")) // You can rebind Oni's behaviour to a new keybinding
  // Input
  //
  // Add input bindings here:
  //
  oni.input.bind("<c-enter>", () => console.log("Control+Enter was pressed"))

  //
  // Or remove the default bindings here by uncommenting the below line:
  //
  // oni.input.unbind("<c-p>")
}

export const deactivate = (oni: Oni.Plugin.Api) => {
  console.log("config deactivated")
}

export const configuration = {
  //add custom config here, such as
  activate,
  "oni.hideMenu": "hidden", // Hide top bar menu
  "oni.loadInitVim": true, // Load user's init.vim
  "oni.useDefaultConfig": false, // Do not load Oni's init.vim
  //"ui.colorscheme": "nord",
  "ui.colorscheme": "gruvbox_dark",
  //"ui.colorscheme": "onedark", // Load init.vim colorscheme, remove this line if wants Oni's default

  "autoClosingPairs.enabled": false, // disable autoclosing pairs
  "commandline.mode": false, // Do not override commandline UI
  "wildmenu.mode": false, // Do not override wildmenu UI,

  //"tabs.mode"                : "native", // Use vim's tabline, need completely quit Oni and restart a few times

  "statusbar.enabled": true, // use vim's default statusline
  "sidebar.enabled": true, // sidebar ui is gone
  "sidebar.default.open": false, // the side bar collapse
  "learning.enabled": true, // Turn off learning pane
  "achievements.enabled": false, // Turn off achievements tracking / UX
  "editor.typingPrediction": false, // Wait for vim's confirmed typed characters, avoid edge cases
  "editor.textMateHighlighting.enabled": false, // Use vim syntax highlighting

  "oni.bookmarks": ["~/Documents"],
  //"oni.loadInitVim": false,
  "editor.fontSize": "12px",
  "editor.fontFamily": "Hack Nerd Font",
  "experimental.markdownPreview.enabled": true,

  // UI customizations
  "ui.animations.enabled": true,
  "ui.fontSmoothing": "auto",
}
