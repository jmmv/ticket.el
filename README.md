# ticket.el

This package provides Emacs integration for the ticket (tk) tracking tool
found in https://github.com/wedow/ticket.

## Setup: Doom Emacs

```elisp
;;; In ~/.doom.d/packages.el
(package! ticket
  :recipe (:host github :repo "jmmv/ticket.el" :files ("ticket.el")))

;;; In ~/.doom.d/config.el
(use-package! ticket
    :config
    (map! :leader :desc "Ticket" "k" #'ticket-transient))
```
