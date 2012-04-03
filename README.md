# XMonad Starter Kit #

## Highlights ##

`M-` for Super/Win, `S-` for Shift, `C-` for Control.

-   Panel for multiple displays. Desktop info is only shown on current screen.
-   Dynamic workspace creation

    -   `M-n`: jump to create new workspace. `S-<Enter>` to force creating
        new window.
    -   `M-S-n`: move window to select workspace, `S-<Enter>` to force creating
        new window.
    -   `M-C-S-n`: rename workspace
    -   `M-C-S-<Backspace>`: delete current workspace if it is empty

-   Shortcuts help extracted from source: `M-c h`
-   Urgency window notification through libnotify 
-   Switch workspace and toggle layouts using mouse.
-   Select layout by prompt: `M-S-\`
-   Multi toggle:

    -   `M-f`: toggle fullscreen
    -   `M-<Escape>`: toggle fullscreen, where hide panel in fullscreen.
    -   `M-d`: zoom in active window
    -   `M-s`: Make master window as sidebar. `M-S-:` can promote a window as
        master window.

-   Carefully configured layouts:

    -   `cols`: number of windows in each column can be changed using
        `M-C-[` and `M-C-]`.
    -   `two`: Only show two windows. `M-a` and `M-S-a` are useful in this layout.
    -   `rows`: Like cols but arrange windows into rows
    -   `tab`: Like fullscreen but with a tab bar showing window names.
    -   `grid`
    -   `big`: the master window occupy most space.

-   `M-g`, `M-b`
    [switch window using fuzzy matching](http://blog.iany.me/2010/08/switch-window-using-fuzz-matching/)

## Install ##

-   See prerequisites and optional tools below. For Arch Linuxer:

        yaourt -S ghc xmonad xmonad-contrib \
          zsh dzen2-xft-xpm-xinerama-svn dmenu-xft \
          rxvt-unicode pcmanfm-mod scrot nitrogen \
          xscreensaver gmrun xdotool wmctrl \
          gsimplecal gpicker \
          wqy-microhei-nightly_build \
          trayer libnotify

-   Add path to `bin` to `PATH`.
-   Copy or link xmonad.hs to `~/.xmonad/xmonad.hs`.

For `startx` and `slim`, copy or link following files to home directory
(backup your own first):

    _xinitrc -> ~/.xinitrc
    _xsession -> ~/.xsession
    _xsessionrc -> ~/.xsessionrc

-   See `_xsessionrc` about how to setup multiple displays using `xrandr`.
-   Add auto start applications in `bin/xmonad.autostart`, such as starting `trayer`.

TODO: How to start session in other session manager? I'm not sure whether
`~/.xsession` really works for xdm/gdm/kdm.

## Prerequisites ##

### XMonad ###

`ghc`, and `xmonad-0.10` `xmonad-contrib-0.10`

In Arch Linux:

    yaourt -S ghc xmonad xmonad-contrib

Or install using cabal:

    yaourt -S cabal
    cabal install xmonad xmonad-contrib

Cabal install packages into `~/.cabal`, add `~/.cabal/bin` to `PATH`.

### Panel ###

#### zsh ####

Install latest `zsh` (mine is 4.3.17). Panel is written in zsh using zselect.

#### dzen2 ####

Must enable xft, xpm and xinerama. in Arch Linux, just install from `aur`:

    yaourt -S dzen2-xft-xpm-xinerama-svn

Other system can compile from svn truck http://dzen.googlecode.com/svn/trunk/
Edit config.mk enable `Option 7`.

#### dmenu ####

Version 4.5 with xft patch. Arch Linuxer can install `dmenu-xft`.

    yaourt -S dmenu-xft

Or compile source after applying
[dmenu-4.5-xft.diff](http://darkstar.ist.utl.pt/slackware/addon/slacky/slackware64-13.37/desktop/dmenu-xft/4.5/src/dmenu-4.5-xft.diff)

## Optional Tools ##

-   `urxvt`: terminal, update `myTerminal` and `myTerminalClass` in xmonad.hs
    to use other terminals.

-   `libnotify`: urgency notification

-   `pcmanfm`: lightweight file browser, Arch can install `pcmanfm-mod`:

        yaourt -S pcmanfm-mod

-   `scrot`: snapshot of whole screen `M-<PrintScreen>`, snapshot of
    selection: `M-C-<PrintScreen>`.

-   `nitrogen`: set desktop background image.

-   `xscreensaver`: screen saver

-   `gmrun`: Launcher for `M-<Enter>`.

-   `xdotool`: switch layouts using mouse
-   `wmctrl`: switch layouts using mouse
-   `gsimplecal`: calendar widget
-   `gpicker`: goto/bring window

-   font WenQuanYi MicroHei (for Chinese), or replace `myFont` and `myMonoFont` in
    `xmonad.hs`.

-   `trayer`: trayer for daemon icons
