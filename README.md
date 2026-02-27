# ticket.el

An Emacs package that provides integration with the
[ticket (tk)](https://github.com/wedow/ticket) command-line ticket tracking
tool.  It offers a transient command menu for common ticket operations and an
interactive tree-based browser for navigating tickets and their dependencies.

## Features

- **Transient menu** — a single entry point (`ticket-transient`) for creating,
  listing, and closing tickets.
- **Ticket browser** — a read-only buffer (`*tickets*`) that renders tickets as
  a collapsible dependency tree, sorted by priority.
- **Inline priority editing** — raise or lower ticket priority directly from the
  browser without opening the file.
- **Filter modes** — show only open/in-progress tickets or all tickets including
  closed ones.
- **Ticket view minor mode** — auto-activates on `.tickets/*.md` files and
  exposes commands for editing status, type, dependencies, and parent without
  leaving the buffer.
- **Evil mode support** — keybindings are also registered in Evil's motion state.

## Prerequisites

- Emacs 28 or later (uses `transient`, which is built-in since Emacs 29; on
  Emacs 28, install it from MELPA).
- The [`tk`](https://github.com/wedow/ticket) command-line tool installed and
  available on your `PATH` as `tk`.

## Installation

### package-vc (Emacs 29+)

```elisp
(package-vc-install "https://github.com/jmmv/ticket.el")
```

### use-package with :vc (Emacs 30+)

```elisp
(use-package ticket
  :vc (:url "https://github.com/jmmv/ticket.el" :rev :newest))
```

### straight.el

```elisp
(straight-use-package
  '(ticket :host github :repo "jmmv/ticket.el" :files ("ticket.el")))
```

### Manual

Clone the repository and add its directory to `load-path`:

```elisp
(add-to-list 'load-path "/path/to/ticket.el")
(require 'ticket)
```

### Doom Emacs

```elisp
;;; In ~/.doom.d/packages.el
(package! ticket
  :recipe (:host github :repo "jmmv/ticket.el" :files ("ticket.el")))

;;; In ~/.doom.d/config.el
(use-package! ticket
  :config
  (map! :leader :desc "Ticket" "k" #'ticket-transient))
```

## Configuration

The only user-facing variable is `ticket-executable`, which controls the path
to the `tk` binary.  It defaults to `tk` on the system `PATH`.  Override it
if the binary lives elsewhere:

```elisp
(setq ticket-executable "/path/to/tk")
```

### Binding the transient menu

Bind `ticket-transient` to a convenient key.  Example using the built-in
`keymap-global-set`:

```elisp
(keymap-global-set "C-c t" #'ticket-transient)
```

## Usage

### Transient menu

Invoke `ticket-transient` (or whatever key you bound it to) to open the menu:

| Key | Action                              |
|-----|-------------------------------------|
| `c` | Create a new ticket                 |
| `l` | Open browser (open tickets only)    |
| `L` | Open browser (all tickets)          |
| `C` | Close a ticket (completing-read)    |

### Ticket browser keybindings

Inside the `*tickets*` browser buffer:

| Key   | Action                                         |
|-------|------------------------------------------------|
| `RET` | Open the ticket file at point                  |
| `TAB` | Toggle expand/collapse of node at point        |
| `<`   | Collapse all nodes                             |
| `>`   | Expand all nodes                               |
| `g`   | Refresh — reload tickets from disk             |
| `s o` | Filter: show open and in-progress tickets only |
| `s a` | Filter: show all tickets including closed      |
| `+`   | Increase priority of ticket at point (P0 = highest) |
| `-`   | Decrease priority of ticket at point           |
| `q`   | Quit the browser window                        |

### Ticket view minor mode

`ticket-view-mode` is a minor mode that activates automatically whenever you
open a file matching `.tickets/*.md`.  It is indicated by the ` Ticket` lighter
in the mode line.

The mode adds commands to manipulate the ticket's fields without leaving the
buffer:

- **Status** — close or reopen the ticket (delegates to `tk`; the buffer is
  reloaded automatically).
- **Type** — switch between `task` and `epic` by editing the frontmatter field
  in place.
- **Dependency** — open the ticket browser in *selection mode*; press `RET` on
  any ticket to record it as a dependency, or `q` to cancel.
- **Parent** — open the browser in selection mode; press `RET` to set the
  chosen ticket as the parent (written directly to the frontmatter).

#### Standard Emacs keybindings (`C-c k` prefix)

| Key           | Action                     |
|---------------|----------------------------|
| `C-c k s d`   | Add dependency             |
| `C-c k s p`   | Set parent                 |
| `C-c k s s c` | Close ticket               |
| `C-c k s s o` | Reopen ticket              |
| `C-c k s t t` | Set type to `task`         |
| `C-c k s t e` | Set type to `epic`         |

#### Evil / transient menu (`SPC k`)

When `ticket-view-mode` is active the transient menu (`ticket-transient`, bound
to `SPC k` in the Doom example) gains a **"Current ticket"** section:

| Key     | Action             |
|---------|--------------------|
| `s d`   | Add dependency     |
| `s p`   | Set parent         |
| `s s c` | Close              |
| `s s o` | Reopen             |
| `s t t` | Set type: task     |
| `s t e` | Set type: epic     |

The "Current ticket" section is absent when `ticket-view-mode` is not active
(i.e., in non-ticket buffers), so `SPC k` continues to work unchanged
everywhere else.

#### Selection mode in the browser

When `ticket-view-set-dep` or `ticket-view-set-parent` opens the browser, the
status bar shows `Select … (RET to confirm, q to cancel)`.  Pressing `RET` on
a ticket calls the appropriate action and returns focus to the source ticket
buffer.  Pressing `q` closes the browser without making any changes.

### Ticket file format

Tickets are Markdown files stored under a `.tickets/` directory at the project
root.  Each file has a YAML frontmatter block followed by a Markdown body:

```markdown
---
id: abc123
status: open
priority: 2
type: task
parent: xyz789
---
# Ticket title

Description goes here.
```

Fields recognized by ticket.el:

| Field      | Values                          | Notes                          |
|------------|---------------------------------|--------------------------------|
| `id`       | string                          | Unique ticket identifier       |
| `status`   | `open`, `in_progress`, `closed` | Default: `open`                |
| `priority` | integer (0 = highest)           | Default: `2`                   |
| `type`     | `task`, `bug`, …                | Default: `task`                |
| `parent`   | another ticket `id`             | Optional; used for tree layout |

### Display conventions

In the browser, each ticket is shown as:

```
<indent><symbol> <id>    [P<n>]  [<type>]  <title>
```

- `▶` — collapsed node with children
- `▼` — expanded node with children
- `•` — leaf node (no children)
- In-progress tickets are highlighted with the `warning` face.
- Closed tickets are rendered with the `shadow` face.
