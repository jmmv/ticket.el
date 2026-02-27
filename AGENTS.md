# Tooling

*   This project uses Jujutsu for version control.

    *   Make sure to use `jj --no-pager` in general so that you don't get stuck in spurious pagers.
    
    *   You cannot `jj abandon` an empty commit that sits at the top of the stack. To make it go away, you have to `jj edit` the parent commit and stop there.
    
    *   You are not allowed to abandon or amend commits (via `jj edit` or `jj squash`) that you yourself did NOT create in this session.

*   This project uses `tk` for task tracking.

    *   Use `tk help` if you need to understand usage syntax.
    
    *   You can find tickets in the `.tickets` directory and read them directly.
    
    *   Do not use `tk edit`; it's not friendly to not having a console. Edit the markdown files directly if you must.

# Process

Before you start working on a change:

*   Start a new commit by issuing `jj new` so that I can review your work, but only do this if you are not _already_ on a brand new (empty) commit.

When you are done making a change, run through this checklist:

*   Use `jj describe -m` and persist a commit message that explains, at a high level, what was changed and why. Avoid bulleted lists of things that changed. Keep the description user-friendly: do not list every minutia about code changes; those are explained by the diff itself. Use standard Git message formatting, with a short first line and paragraphs that don't exceed 72-75 characters.

*   Update the `README.md` if necessary.

*   If I asked you to work on a task, mark the task as closed with `tk close`.
