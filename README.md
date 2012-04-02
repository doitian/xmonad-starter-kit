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

#### ruby ####

Install `ruby` and gems `eventmachine`, `popen4`.

#### optional ####

-   `xdotool`: switch layouts using mouse
-   `wmctrl`: switch layouts using mouse
-   `gsimplecal`: calendar widget
-   `gpicker`: goto/bring window

### Other Tools ###

-   `urxvt`: terminal

-   `pcmanfm`: lightweight file browser, Arch can install `pcmanfm-mod`:

        yaourt -S pcmanfm-mod

-   `scrot`: snapshot of whole screen `M-<PrintScreen>`, snapshot of
    selection: `M-C-<PrintScreen>`.

-   `xscreensaver`: screen saver

## Install ##

-   Add path to `bin` to `PATH`.
-   Copy or link xmonad.hs to `~/.xmonad/xmonad.hs`.
-   Merge `_Xresources` to `~/.Xresources`.


For `startx` and `slim`:

-   Edit and copy files starting with underscore to home directory:

        _xinitrc -> ~/.xinitrc
        _xsession -> ~/.xsession
        _xsessionrc -> ~/.xsessionrc

TODO: How to start session in other session manager? I'm not sure whether
`~/.xsession` really works for xdm/gdm/kdm.
