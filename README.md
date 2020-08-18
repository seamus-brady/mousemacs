# Mousemacs

Mousemacs is an Emacs setup for those who want the power of Emacs but also want to use a mouse.

Mousemacs comes with the following:
- Standard keyboard shortcuts for cut/copy/paste.
- Similar keyboard shortcuts to Visual Studo Code or Sublime Text.
- File open in tabs and can be closed with a menu.
- A useful context menu with useful shortcuts.
- A built in file explorer (NeoTree).

Underneath Mousemacs is still Emacs.

## Install

- Back up your current emacs config.
- Git clone Mousemacs as below:
```git clone https://github.com/corvideon/mousemacs.git ~/.emacs.d```
- Mousemacs will take a few moments to download and install all packages.


## Key Bindings

- Standard key bindings for cut/copy/paste work.
- Switch tabs with Ctrl-Tab.
- Home/End work as expected.

| Keys   |      Function    |
|----------|:-------------:|
| Ctrl-s |  Save file buffer |
| Ctrl-a |    Select all   |
| Ctrl-n | New empty file buffer |
| Ctrl-o | Open file |
| Ctrl-k | Close file buffer |
| Ctrl-Shift-b | Open file buffer list |
| Ctrl-Shift-r | Open recent file list |
| Ctrl-q | Quit Emacs |
| Ctrl-w | Switch window |
| Ctrl-f | Fine in buffer (Swiper) |
| Ctrl-Shift-p or Standard Emacs M-x | Run command |
| Ctrl-r | Goto anything (Emacs command is imenu-anywhere)|
| Ctrl-e or F8 | Toggle file explorer|
| Ctrl-b | Open other file buffer |




## Screenshots

The main interface with file explorer. Note the friendly 'Files' button on the menu to toggle it:

![Screenshot](/images/screenshot.png)

The file tabs:

![Tabs](/images/tabs.png)

The context menu available on the tab bar:

![Tab menu](/images/tab_menu.png)

The context menu which has shortcuts to lots of useful things such as window tools, buffer lists and more:

![Context menu](/images/context_menu.png)
