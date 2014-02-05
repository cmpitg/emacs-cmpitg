# cmpitg's personal Emacs config #

## Introduction ##

My specialized Emacs configuration, started as
[a clone of my config](https://github.com/cmpitg/emacs-config) as of February
2014.

## Ideas ##

* Package configurations are managed with *recipes*.

* Tightly integrated with my current environment: TODO: screenshots.
  - Window manager: [i3 window manager](http://i3wm.org/). TODO: config.
  - Web browser: Firefox, with `-no-remote` and multiple profiles.
  - Panel: XFCE4 panel.
  - Music player: Audacious.
  - Main language: [Racket](http://racket-lang.org).

### Recipes ###

```
emacs-cmpitg/
  recipes/
    package1.el
    package2.el
    ...
    disabled-packages.el
```

* A recipe is a configuration set for one package.

* Reside in `./recipes/`.

* Follow the filenaming convention: `./recipes/package-name.el`.

* Are automatically loaded, unless `package-name` is in
  [`./recipes/disabled-packages.el`](./recipes/disabled-packages.el).

## Screenshot ##

## Requirements ##

* A \*nix system

* Emacs 24+

* For the GUI file browser:

  - Emacs is running under server mode with default socket path.  This could
    be achieved by using the `init.el` config or simply evaluating
    `(server-start)` in your Emacs.

  - Python 3 PyQt (coming soon)

* For file opening: zenity (GTK+ GUI dialog, used for file choosing)

* For `ibus-mode`: `python-xlib` package.

* For Python development:
  - Packages `jedi epc` for auto-completion.

TODO

## Detailed Description ##

* Some selective libraries are *always loaded* by default, including but not
  limited to:

  - [The missing hashtable library for Emacs](https://github.com/Wilfred/ht.el) `ht.el`
  - [Better APIs for string processing](https://github.com/magnars/s.el) `s.el`
  - [Better APIs for file and directory processing](https://github.com/rejeep/f.el) `f.el`
  - [Modern functional APIs for Emacs](https://github.com/magnars/dash.el) `dash`
  - [Multiple cursors](https://github.com/magnars/multiple-cursors.el)

  See `$HOME/emacs-config/init-package-manager.el` for the full list of
  pre-installed packages.

* All `config-` files in your `$HOME/emacs-config/config-default` is
  automatically `require`d, unless the package name appears in the list
  `*disabled-package-list*`.

* [ErgoEmacs](http://ergoemacs.org/) keybindings, with `<Super>` key as the
  modifier.

* Optimized for
  [Programmer Dvorak key layout](http://www.kaufmann.no/roland/dvorak/index.html).

* Making extensive use of mouse and `<Super>` key.

* "When in doubt, leave it out".  Use only what I need.

* Opening file with a GUI dialog (provided by Zenity, defined in
  `~/emacs-config/config-default/custom-functions.el`).

* Maintainable.

* Automatically `chmod +x` shebang-ed files.

* All temporary buffers (buffers started and ended with `*`, such as `*Help*`,
  `*Completions*`, `*Messages*`, ...) are treated as popup buffer using
  [popwin](https://github.com/m2ym/popwin-el) extension.

### The GUI File Browser ###

The file browser is written in `PySide` (Python wrapper for Qt framework).
Source code of the file browser is distributed under the terms of the GNU
General Public License version 3.0.

Features:

* Files and directories are displayed as a tree:
  - Double click an item it in your Emacs server
  - Right click an item to open the context menu:
    + Browse the current item if it's a directory
    + Copy full path
    + Copy file/directory name
    + Delete file/directory from your drive

* Current visited path is displayed in the first text box called the
  *pathbar*.  Supported format includes `~` and shell variables (such as
  `$HOME`, `$SOMEDIR`).  Shortcut to go directly to the *pathbar*: `Ctrl+L`.

* User could modify directly the *pathbar* or click on the left-top browse
  button to visit a directory.  Shortcut to open directory browser dialog:
  `Ctrl+O`.

* The displayed files/directories could be filter by name with the regular
  expression in the second text box (called *filterbar*).  Shortcut to go
  directly to the *filterbar*: `Ctrl-F`.

## Use Cases with Keybindings ##

TODO: Making nice table with: Keybinding - Description - Function - Provided by

* Use **mouse effectively**, [Acme mouse chords](http://acme.cat-v.org/mouse):
  - Selecting text by dragging *button one*, keep holding down *button one*:
    * Press *button two* to cut
    * Press *button three* to paste
  - When some text is selected, press *button three* to search for selection
    in the current buffer.

  ![Acme mouse chords in Emacs](http://i.imgur.com/H0Xh8RG.png)

  Original image is at [acme.cat-v](http://acme.cat-v.org/mouse).

* What does this keybinding do? `C-h k [keybinding]`

* Description for this function and its keybinding? `C-h f [function-name]`

* Emacs Lisp: support
  [modern ways to process lists](https://github.com/magnars/dash.el).

* Scratch/temporary buffer:
  - Switch default scratch buffer: `C-f1`
  - New scratch buffer: `C-x C-n`

* Open file:
  - Recent file: `<M-f4>`
  - Open file using fuzzy matching: `f3`
  - Open file with Helm: `<M-f3>`:
    * `C-l` to go up one level
    * `C-z` to go down one level
  - Open current file as root, using `sudo`: `s-z`
  - Close current buffer: `<C-f4>`
  - `<return>` in any file browsing mode to smartly open file with Emacs or
    external application (thanks to the excellent
    [`openwith` extension](http://www.logic.at/prolog/misc/openwith.el))

* Bookmarks:
  - Go to bookmark: `S-f8`
  - List bookmarks: `C-x r l`
    * `d` to delete bookmark
    * `x` to commit deletions
  - Add to bookmark: `C-x r m`
  - Recent files: `<M-f4>` (thanks to
    [`recentf`](http://www.emacswiki.org/emacs/RecentFiles))

* Window management:
  - Delete all other windows: `C-%`
  - Hide current window: `<S-f4>`
  - Split vertically: `C-7`
  - Split horizontally: `C-5`
  - Hide [popup](https://github.com/m2ym/popwin-el) window: `C-g`
  - Scroll other window down: `C-M-v`
  - Scroll other window up: `C-S-M-v`

* Buffer management:
  - Show current buffer list: `f8` (with helm) or `<C-f2>` (with `buffer-menu`)
  - Kill current buffer: `C-delete` or `C-f4`
  - Switch back and forth between 2 most recent buffers: `s-B`

* Movement:
  - Set a mark (to jump): `C-SPC C-SPC`
  - Come back to the last mark or point where you just editted: `C-u C-SPC`
  - List all occurrences of an expression in current buffer: `s-s` (`helm-occur`)

* Basic text processing:
  - Upcase word, lowcase word, and capitalize word: `M-u`, `M-l`, and `M-c`
  - Find next/prev occurrences, use one of these:
    * *Button three* click on a *selection* (see Acme mouse chords)
    * Go to the prev/next occurrence of the symbol at cursor: `M-p`/`M-n`
      (feature provided by
      [smartscan](http://www.masteringemacs.org/articles/2013/10/31/smart-scan-jump-symbols-buffer/))
  - Find all occurrences of a regular expression in all opened files: `<f4>`
  - Surround text: `s-SPC s`
  - In Markdown mode: Use `s-SPC i`, `s-SPC b`, and `s-SPC r` to italicize,
    embolden, or rawify text selection.

* Completion:
  - Using `pabbrev` only: `<s-return>`
  - Using `autocomplete` or `pabbrev`: `<tab>`
  - Using `hippie-expand` (fuzzy completion with guessing): `M-/`

* Yasnippet:
  - Visit snippet file: `C-c & C-v`
  - When in `snippet-mode`:
    * Tryout snippet: `C-c C-t`
    * Load snippet: `C-c C-c`

* `grep`ing:
  - `grep`ing could be done with `ack-and-a-half` via `ack` or `<C-f10>`
  - Ack buffer is edittable and saveable thanks to
    [`wgrep-ack`](https://github.com/mhayashi1120/Emacs-wgrep):
    * To start editting: `C-c C-p`
    * To save changes: `C-x C-s`
    * To discard changes: `C-x C-k`
    * To commit changes to files: `C-c C-e` or `wgrep-save-all-buffers`

### With Any Interactive mode ###

The following keybindings are applied to when you want to interative with a
REPL.

Currently supported REPL:

* [MozRepl](https://github.com/bard/mozrepl/wiki) (for JavaScript)
* [Pry](http://pryrepl.org/) (for Ruby).
* [Geiser](http://www.nongnu.org/geiser/) for Scheme and
  [Racket development](http://docs.racket-lang.org/guide/Emacs.html).
* Python (built-in).

General keybindings:

* Invoke and/or jump to REPL: `C-c C-z` or `C-c C-i`
* Eval last expression: `C-x C-e` or `C-c C-e`
* Eval region: `C-c C-r`
* Eval buffer: `C-c C-b` or `C-c C-c` (in some modes)
* Eval function: `C-M-x` or `C-c C-c` (in some modes)
* Show documentation of current word/symbol/identifier: `f1`

#### With Python Development ####

(TODO) Making screencast, solving a Project Euler problem

#### With Lisp Development ####

* Transpose 2 s-expression with `C-M-t`.  E.g.

  ```scheme
  '(1 |2)         ;; => '(2 1|)
  '((1 2 3) |4)   ;; => '(4 (1 2 3)|)
  ```

#### With Racket Development ####

(TODO) Making screencast, solving a Project Euler problem

#### `eshell` ####

* Start/switch back and forth to eshell: `switch-to-eshell-back-and-forth`

* Display eshell history with `ido`, choosing history adds the command to
  current eshell buffer: `eshell-history`

#### File Management with Sunrise Commander ####

* Open Sunrise: `s-SPC SPC`
* Sunrise change dir: `s-SPC c`
* In Dired mode, to toggle detail information: `(` or `)`

In Sunrise mode:

* Hide details: `C-backspace`
* Prefixing copy/cut commands with `C-u` does the action in the background (a
  feature provided by
  [`sunrise-x-loop`](http://www.emacswiki.org/emacs/sunrise-x-loop.el)
  package)

(TODO) Making screencast with use cases:

* Mass renaming with `wdired`
* Mass copy/moving
* Shell commands
* Open file with external program
* Tree browsing

#### Working with Git ####

Cheatsheet: http://daemianmack.com/magit-cheatsheet.html

Git should be setup with SSH.

Git status: `s-SPC g`

- Quit: `q`

- Committing:
  * Previous hunk, next hunk: `p`, `n`
  * Stage/unstage current hunk: `s`/`u`
  * Stage/unstage all hunks: `S`/`U`
  * Ignore file: `i`
  * Toggle visibility: `tab`
  * Toggle visibility of all: `S-tab`
  * Reload buffer: `g`

- Commit: `c`
  * Execute commit: `C-c C-c`

- History and verbose history: `l`, `L`

- Copy SHA1: `C-w`

- Marking:
  * Mark/unmark current commit: `..`/`C-u ..`
  * Toggle commit marking: `.`

- Diff-ing:
  *Show diff between marked and current commit: `=`

- Reseting:
  * Current head: `x`
  * Hard reset, **destructive**: `X`

- Pushing & pulling & rebasing:
  * Push: `P`
  * Pull: `F`
  * Rebase: `R`

- Branching:
  * Switch branch: `b`
  * Create and switch branch: `B`

#### Github ####

It's best to config your
[Git environment for Github](https://github.com/blog/180-local-github-config)
first.  Current my Emacs uses [@defunkt](https://github.com/defunkt)'s
[gist.el](https://github.com/defunkt/gist.el)

* `gist-region-or-buffer`
* `gist-region-or-buffer-private`

## License ##

Except for packages which don't belong to this configuration in the first
place, and unless clearly stated, all the code in this configuration is
distributed in terms of the GNU General Public License version 3 (GPL v3).
See `COPYING` for further information.

## TODOs ##

Refactor this document into simple use cases/tasks.

* `s-v` to go to package manager's package list (`package-list-packages`)

* `s-\` to toggle `ibus-mode` (`ibus-mode`), then `C-M-S-SPC` to toggle Ibus
  (`$toggle-ibus`)

* Code:
  - `C--` to toggle comment on selection (`'$toggle-comment-region`)

* Toggle whitespace visibility `C-<menu> C-w`:
  - Delete redundant whitespaces `s-w`
