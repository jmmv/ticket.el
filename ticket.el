;;; ticket.el --- A simple ticket management tool using `tk`  -*- lexical-binding: t -*-

(require 'seq)
(require 'cl-lib)

(defun ticket--root-directory ()
  "Return the project root (directory containing .tickets/) or nil."
  (locate-dominating-file default-directory ".tickets/"))

(defun ticket-directory ()
  "Return the path to the .tickets directory for the current project."
  (let ((root (ticket--root-directory)))
    (when root
      (expand-file-name ".tickets/" root))))

(defgroup ticket ()
  "A simple ticket management tool using `tk`."
  :group 'applications)

(defcustom ticket-executable
  (executable-find "tk")
  "The path to the 'tk' executable.
Defaults to 'tk' found in the path."
  :type 'string
  :group 'ticket)

(defun ticket--list-tickets ()
  "Return a list of open tickets."
  (let ((default-directory (or (ticket--root-directory) default-directory)))
    (with-temp-buffer
      (shell-command (format "%s list" ticket-executable) (current-buffer))
      (goto-char (point-min))
      (let ((tickets ()))
        (while (not (eobp))
          (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                 (parts (split-string line " " t))
                 (id (car parts))
                 (title (string-join (cdr (cdr parts)) " ")))
            (unless (string-match-p "^ID" id)
              (push (cons (format "%s: %s" id title) id) tickets)))
          (forward-line))
        (nreverse tickets)))))

;;;###autoload
(defun ticket-create (title)
  "Create a new ticket with TITLE and open it for editing."
  (interactive "sTicket title: ")
  (let* ((default-directory (or (ticket--root-directory) default-directory))
         (ticket-id (string-trim (shell-command-to-string (format "%s create \"%s\"" ticket-executable title))))
         (ticket-file (expand-file-name (concat ticket-id ".md") (ticket-directory))))
    (find-file ticket-file)
    (goto-char (point-max))))

;;;###autoload
(defun ticket-close ()
  "Select a ticket to close."
  (interactive)
  (let* ((default-directory (or (ticket--root-directory) default-directory))
         (tickets (ticket--list-tickets))
         (choice (completing-read "Close ticket: " tickets nil t))
         (ticket-id (cdr (assoc choice tickets))))
    (when ticket-id
      (shell-command (format "%s close %s" ticket-executable ticket-id))
      (message "Ticket %s closed." ticket-id))))

;; ==================== ticket-browser ====================

(defvar-local ticket-browser--tickets nil
  "List of all loaded ticket plists.")

(defvar-local ticket-browser--expanded nil
  "Hash table mapping ticket id to t (expanded) or nil (collapsed).")

(defvar-local ticket-browser--filter 'open-only
  "Current filter: `all' or `open-only'.")

(defvar-local ticket-browser--selection-callback nil
  "When non-nil, a function called with a ticket ID instead of
opening it.  Used by `ticket-view-set-dep'.")

(defun ticket-browser--parse-file (file)
  "Parse ticket FILE. Returns a plist or nil."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (looking-at "^---[ \t]*$")
      (forward-line)
      (let ((frontmatter-start (point)))
        (when (re-search-forward "^---[ \t]*$" nil t)
          (let ((fm (buffer-substring-no-properties frontmatter-start (match-beginning 0)))
                (id nil) (status nil) (priority nil) (type nil) (parent nil))
            (dolist (line (split-string fm "\n"))
              (cond
               ((string-match "^id:[ \t]*\\(.*\\)$" line)
                (setq id (string-trim (match-string 1 line))))
               ((string-match "^status:[ \t]*\\(.*\\)$" line)
                (setq status (string-trim (match-string 1 line))))
               ((string-match "^priority:[ \t]*\\(.*\\)$" line)
                (setq priority (string-trim (match-string 1 line))))
               ((string-match "^type:[ \t]*\\(.*\\)$" line)
                (setq type (string-trim (match-string 1 line))))
               ((string-match "^parent:[ \t]*\\(.*\\)$" line)
                (setq parent (string-trim (match-string 1 line))))))
            ;; Title comes from the first # heading in the body after frontmatter
            (let ((title ""))
              (when (re-search-forward "^# \\(.+\\)$" nil t)
                (setq title (string-trim (match-string 1))))
              (when id
                (list :id id
                      :status (or status "open")
                      :priority (or priority "2")
                      :type (or type "task")
                      :parent parent
                      :title title
                      :file file)))))))))

(defun ticket-browser--load-tickets ()
  "Return list of ticket plists from the .tickets/ directory."
  (let ((dir (ticket-directory)))
    (when dir
      (let ((tickets '()))
        (dolist (file (directory-files dir t "\\.md$"))
          (let ((parsed (ticket-browser--parse-file file)))
            (when parsed
              (push parsed tickets))))
        tickets))))

(defun ticket-browser--build-graph (tickets)
  "Return (id-table . children-table) for TICKETS.
id-table maps id to ticket plist.
children-table maps id to list of child ids (tickets whose parent is this id)."
  (let ((id-table (make-hash-table :test 'equal))
        (children-table (make-hash-table :test 'equal)))
    (dolist (ticket tickets)
      (puthash (plist-get ticket :id) ticket id-table))
    (dolist (ticket tickets)
      (let ((id (plist-get ticket :id))
            (parent (plist-get ticket :parent)))
        (when parent
          (puthash parent (append (gethash parent children-table '()) (list id)) children-table))))
    (cons id-table children-table)))

(defun ticket-browser--get-roots (tickets id-table)
  "Return list of root ticket IDs.
Roots are tickets with no parent, or whose parent is unknown (not in id-table)."
  (let ((roots '()))
    (dolist (ticket tickets)
      (let ((id (plist-get ticket :id))
            (parent (plist-get ticket :parent)))
        (when (or (null parent) (not (gethash parent id-table)))
          (push id roots))))
    (nreverse roots)))

(defun ticket-browser--sort-ids-by-priority (ids id-table)
  "Sort IDS by ticket priority; lower priority number appears first."
  (sort (copy-sequence ids)
        (lambda (a b)
          (let ((pa (let ((ta (gethash a id-table)))
                      (if ta (string-to-number (or (plist-get ta :priority) "2")) 2)))
                (pb (let ((tb (gethash b id-table)))
                      (if tb (string-to-number (or (plist-get tb :priority) "2")) 2))))
            (< pa pb)))))

(defun ticket-browser--render-tree (tickets graph filter)
  "Render the ticket tree into the current buffer.
GRAPH is (id-table . children-table). FILTER is `all' or `open-only'."
  (let* ((id-table (car graph))
         (children-table (cdr graph))
         (expanded ticket-browser--expanded)
         (roots (ticket-browser--get-roots tickets id-table))
         (visited (make-hash-table :test 'equal)))
    (cl-labels
        ((mark-visited (id)
           (unless (gethash id visited)
             (puthash id t visited)
             (dolist (child (gethash id children-table '()))
               (mark-visited child))))
         (should-show (ticket)
           (or (eq filter 'all)
               (member (plist-get ticket :status) '("open" "in_progress"))))
         (render-node (id depth)
           (let ((ticket (gethash id id-table)))
             (when (and ticket (not (gethash id visited)))
               (puthash id t visited)
               (when (should-show ticket)
                 (let* ((children (gethash id children-table '()))
                        (visible-children
                         (if (eq filter 'all)
                             children
                           (seq-filter
                            (lambda (cid)
                              (let ((ct (gethash cid id-table)))
                                (and ct (should-show ct))))
                            children)))
                        (has-children (not (null visible-children)))
                        (is-expanded (gethash id expanded))
                        (prefix (make-string (* depth 2) ?\s))
                        (symbol (cond
                                 ((not has-children) "•")
                                 (is-expanded "▼")
                                 (t "▶")))
                        (status (plist-get ticket :status))
                        (priority (plist-get ticket :priority))
                        (type (plist-get ticket :type))
                        (title (plist-get ticket :title))
                        (face (cond
                               ((string= status "closed") 'shadow)
                               ((string= status "in_progress") 'warning)
                               (t 'default)))
                        (line-start (point)))
                   (insert (propertize
                            ;; id column: depth indent + symbol + space + id,
                            ;; padded to 16 chars (max depth 3 = 6 indent + 2 + 8).
                            ;; priority + type columns follow at fixed positions;
                            ;; title is padded to a fixed absolute column (35).
                            (let* ((id-col (format "%-16s" (concat prefix symbol " " id)))
                                   (meta (format "%s  [P%s]  [%s]" id-col priority type))
                                   (pad (make-string (max 2 (- 35 (length meta))) ?\s)))
                              (if (string-empty-p title)
                                  (string-trim-right meta)
                                (concat meta pad title)))
                            'face face))
                   (insert "\n")
                   (add-text-properties line-start (point)
                                        (list 'ticket-browser-id id
                                              'ticket-browser-depth depth
                                              'ticket-browser-has-children has-children))
                   (if (and has-children is-expanded)
                       (dolist (child-id (ticket-browser--sort-ids-by-priority visible-children id-table))
                         (render-node child-id (1+ depth)))
                     ;; Collapsed: mark entire subtree visited so the fallback
                     ;; loop doesn't surface children at the root level.
                     (dolist (child-id children)
                       (mark-visited child-id)))))))))
      (dolist (root (ticket-browser--sort-ids-by-priority roots id-table))
        (render-node root 0))
      ;; Render any unvisited tickets (e.g., whose parent is filtered out)
      (dolist (ticket tickets)
        (let ((id (plist-get ticket :id)))
          (unless (gethash id visited)
            (render-node id 0)))))))

(defun ticket-browser--redisplay ()
  "Re-render the ticket browser buffer."
  (let* ((inhibit-read-only t)
         (id-at-point (get-text-property (point) 'ticket-browser-id))
         (tickets ticket-browser--tickets)
         (graph (ticket-browser--build-graph tickets)))
    (erase-buffer)
    (ticket-browser--render-tree tickets graph ticket-browser--filter)
    ;; Try to restore cursor to same ticket id, else go to start
    (goto-char (point-min))
    (when id-at-point
      (let ((found nil))
        (while (and (not found) (not (eobp)))
          (if (equal (get-text-property (point) 'ticket-browser-id) id-at-point)
              (setq found t)
            (forward-line)))
        (unless found
          (goto-char (point-min)))))))

(defun ticket-browser--ticket-at-point ()
  "Return the ticket ID at point, or nil."
  (get-text-property (point) 'ticket-browser-id))

;;;###autoload
(defun ticket-browser-open-ticket ()
  "Open ticket at point, or call selection callback if set."
  (interactive)
  (let ((id (ticket-browser--ticket-at-point)))
    (when id
      (if ticket-browser--selection-callback
          (let ((callback ticket-browser--selection-callback))
            (setq ticket-browser--selection-callback nil)
            (quit-window)
            (funcall callback id))
        ;; Normal: open the ticket file and position point
        (let ((file (expand-file-name (concat id ".md") (ticket-directory))))
          (when (file-exists-p file)
            (find-file file)
            (goto-char (point-min))
            (if (re-search-forward "^# " nil t)
                (progn
                  (end-of-line)
                  (forward-line)
                  (skip-chars-forward " \t\n\r"))
              (goto-char (point-min))
              (when (search-forward "---" nil t)
                (when (search-forward "---" nil t)
                  (forward-line)
                  (beginning-of-line))))))))))

;;;###autoload
(defun ticket-browser-toggle ()
  "Toggle expand/collapse of node at point."
  (interactive)
  (let ((id (ticket-browser--ticket-at-point)))
    (when (and id (get-text-property (point) 'ticket-browser-has-children))
      (puthash id (not (gethash id ticket-browser--expanded)) ticket-browser--expanded)
      (ticket-browser--redisplay))))

;;;###autoload
(defun ticket-browser-collapse-all ()
  "Collapse all nodes."
  (interactive)
  (maphash (lambda (k _v) (puthash k nil ticket-browser--expanded))
           ticket-browser--expanded)
  (ticket-browser--redisplay))

;;;###autoload
(defun ticket-browser-expand-all ()
  "Expand all nodes."
  (interactive)
  (dolist (ticket ticket-browser--tickets)
    (puthash (plist-get ticket :id) t ticket-browser--expanded))
  (ticket-browser--redisplay))

;;;###autoload
(defun ticket-browser-show-open-only ()
  "Filter to open/in_progress tickets only."
  (interactive)
  (setq ticket-browser--filter 'open-only)
  (ticket-browser--redisplay))

;;;###autoload
(defun ticket-browser-show-all ()
  "Show all tickets including closed."
  (interactive)
  (setq ticket-browser--filter 'all)
  (ticket-browser--redisplay))

;;;###autoload
(defun ticket-browser-refresh ()
  "Reload tickets from disk and re-render."
  (interactive)
  (setq ticket-browser--tickets (ticket-browser--load-tickets))
  (ticket-browser--redisplay)
  (message "Tickets refreshed."))

(defun ticket-browser--set-priority (id new-priority)
  "Set priority of ticket ID to NEW-PRIORITY by editing its .md file directly."
  (let ((file (expand-file-name (concat id ".md") (ticket-directory))))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (when (looking-at "^---[ \t]*$")
          (forward-line)
          (let ((fm-start (point)))
            (when (re-search-forward "^---[ \t]*$" nil t)
              (let ((fm-end (match-beginning 0)))
                (goto-char fm-start)
                (if (re-search-forward "^priority:[ \t]*.*$" fm-end t)
                    (replace-match (format "priority: %d" new-priority))
                  (goto-char fm-end)
                  (insert (format "priority: %d\n" new-priority)))))))
        (write-region (point-min) (point-max) file nil 'silent)))))

;;;###autoload
(defun ticket-browser-increase-priority ()
  "Increase priority of ticket at point (decrease number, min P0)."
  (interactive)
  (let ((id (ticket-browser--ticket-at-point)))
    (when id
      (let* ((ticket (seq-find (lambda (tkt) (equal (plist-get tkt :id) id))
                               ticket-browser--tickets))
             (current (string-to-number (or (plist-get ticket :priority) "2")))
             (new-priority (max 0 (1- current))))
        (ticket-browser--set-priority id new-priority)
        (ticket-browser-refresh)
        (message "Ticket %s priority: P%d" id new-priority)))))

;;;###autoload
(defun ticket-browser-decrease-priority ()
  "Decrease priority of ticket at point (increase number, no limit)."
  (interactive)
  (let ((id (ticket-browser--ticket-at-point)))
    (when id
      (let* ((ticket (seq-find (lambda (tkt) (equal (plist-get tkt :id) id))
                               ticket-browser--tickets))
             (current (string-to-number (or (plist-get ticket :priority) "2")))
             (new-priority (1+ current)))
        (ticket-browser--set-priority id new-priority)
        (ticket-browser-refresh)
        (message "Ticket %s priority: P%d" id new-priority)))))

(define-derived-mode ticket-browser-mode special-mode "Tickets"
  "Major mode for browsing tickets as a dependency tree.")

(define-key ticket-browser-mode-map (kbd "RET") 'ticket-browser-open-ticket)
(define-key ticket-browser-mode-map (kbd "TAB") 'ticket-browser-toggle)
(define-key ticket-browser-mode-map (kbd "<") 'ticket-browser-collapse-all)
(define-key ticket-browser-mode-map (kbd ">") 'ticket-browser-expand-all)
(define-key ticket-browser-mode-map (kbd "g") 'ticket-browser-refresh)
(define-key ticket-browser-mode-map (kbd "q") 'quit-window)
(define-key ticket-browser-mode-map (kbd "+") 'ticket-browser-increase-priority)
(define-key ticket-browser-mode-map (kbd "-") 'ticket-browser-decrease-priority)
(let ((s-map (make-sparse-keymap)))
  (define-key s-map "o" 'ticket-browser-show-open-only)
  (define-key s-map "a" 'ticket-browser-show-all)
  (define-key ticket-browser-mode-map "s" s-map))
;; evil-define-key is a macro and can't be called safely at byte-compile time;
;; use evil-define-key* (the underlying function) inside with-eval-after-load.
(with-eval-after-load 'evil
  (let ((s-map (make-sparse-keymap)))
    (define-key s-map "o" 'ticket-browser-show-open-only)
    (define-key s-map "a" 'ticket-browser-show-all)
    (evil-define-key* 'motion ticket-browser-mode-map
      (kbd "RET") #'ticket-browser-open-ticket
      (kbd "TAB") #'ticket-browser-toggle
      "<" #'ticket-browser-collapse-all
      ">" #'ticket-browser-expand-all
      "g" #'ticket-browser-refresh
      "q" #'quit-window
      "s" s-map
      "+" #'ticket-browser-increase-priority
      "-" #'ticket-browser-decrease-priority)))

(defun ticket-browser--open (filter)
  "Open the ticket browser with FILTER (`open-only' or `all')."
  (let ((buffer (get-buffer-create "*tickets*")))
    (with-current-buffer buffer
      (ticket-browser-mode)
      (setq ticket-browser--tickets (ticket-browser--load-tickets))
      (setq ticket-browser--expanded (make-hash-table :test 'equal))
      (setq ticket-browser--filter filter)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (ticket-browser--render-tree
         ticket-browser--tickets
         (ticket-browser--build-graph ticket-browser--tickets)
         ticket-browser--filter))
      (goto-char (point-min)))
    (switch-to-buffer-other-window buffer)))

;;;###autoload
(defun ticket-browser ()
  "Open the ticket browser showing open/in_progress tickets."
  (interactive)
  (ticket-browser--open 'open-only))

;;;###autoload
(defun ticket-browser-all ()
  "Open the ticket browser showing all tickets including closed."
  (interactive)
  (ticket-browser--open 'all))

(require 'transient)

;;;###autoload
(transient-define-prefix ticket-transient ()
  "Ticket management commands."
  ["Ticket"
   ("c" "Create ticket" ticket-create)
   ("l" "List open tickets" ticket-browser)
   ("L" "List all tickets" ticket-browser-all)
   ("C" "Close ticket" ticket-close)]
  ["Current ticket"
   :if (lambda () (bound-and-true-p ticket-view-mode))
   ("s d"   "Add dependency"  ticket-view-set-dep)
   ("s p"   "Set parent"      ticket-view-set-parent)
   ("s s c" "Close"           ticket-view-set-status-closed)
   ("s s o" "Reopen"          ticket-view-set-status-open)
   ("s t t" "Set type: task"  ticket-view-set-type-task)
   ("s t e" "Set type: epic"  ticket-view-set-type-epic)])

;; ==================== ticket-view-mode ====================

(defvar ticket-view-mode-map
  (let ((map (make-sparse-keymap)))
    (let ((s-map (make-sparse-keymap)))
      (let ((ss-map (make-sparse-keymap))
            (st-map (make-sparse-keymap)))
        (define-key ss-map "c" #'ticket-view-set-status-closed)
        (define-key ss-map "o" #'ticket-view-set-status-open)
        (define-key st-map "t" #'ticket-view-set-type-task)
        (define-key st-map "e" #'ticket-view-set-type-epic)
        (define-key s-map "d" #'ticket-view-set-dep)
        (define-key s-map "p" #'ticket-view-set-parent)
        (define-key s-map "s" ss-map)
        (define-key s-map "t" st-map))
      (define-key map (kbd "C-c k s") s-map))
    map)
  "Keymap for `ticket-view-mode'.")

(define-minor-mode ticket-view-mode
  "Minor mode active when visiting a ticket file under .tickets/.
Provides keybindings to manipulate the ticket's fields without
leaving the buffer."
  :lighter " Ticket")

(defun ticket-view--maybe-activate ()
  "Activate `ticket-view-mode' if the current file is a ticket."
  (when (and buffer-file-name
             (string-match-p "/\\.tickets/[^/]+\\.md\\'" buffer-file-name))
    (ticket-view-mode 1)))

(add-hook 'find-file-hook #'ticket-view--maybe-activate)

(defun ticket-view--current-id ()
  "Return the ticket ID for the current buffer (from its filename)."
  (file-name-sans-extension
   (file-name-nondirectory buffer-file-name)))

(defun ticket-view--set-frontmatter-field (field value)
  "Set FIELD to VALUE in the YAML frontmatter of the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (looking-at-p "^---[ \t]*$")
      (forward-line)
      (let ((fm-start (point)))
        (when (re-search-forward "^---[ \t]*$" nil t)
          (let ((fm-end (match-beginning 0)))
            (goto-char fm-start)
            (when (re-search-forward
                   (format "^%s:[ \t]*.*$" (regexp-quote field))
                   fm-end t)
              (replace-match (format "%s: %s" field value)))))))))

(defun ticket-view-set-status-closed ()
  "Close the current ticket."
  (interactive)
  (let ((id (ticket-view--current-id))
        (default-directory (or (ticket--root-directory) default-directory)))
    (shell-command (format "%s close %s" ticket-executable id))
    (revert-buffer nil t)
    (message "Ticket %s closed." id)))

(defun ticket-view-set-status-open ()
  "Reopen the current ticket."
  (interactive)
  (let ((id (ticket-view--current-id))
        (default-directory (or (ticket--root-directory) default-directory)))
    (shell-command (format "%s reopen %s" ticket-executable id))
    (revert-buffer nil t)
    (message "Ticket %s reopened." id)))

(defun ticket-view-set-type-task ()
  "Set the current ticket's type to 'task'."
  (interactive)
  (ticket-view--set-frontmatter-field "type" "task")
  (save-buffer)
  (message "Type set to 'task'."))

(defun ticket-view-set-type-epic ()
  "Set the current ticket's type to 'epic'."
  (interactive)
  (ticket-view--set-frontmatter-field "type" "epic")
  (save-buffer)
  (message "Type set to 'epic'."))

(defun ticket-view-set-dep ()
  "Add a dependency to the current ticket via the ticket browser.
Opens the browser in selection mode; press RET on a ticket to add
it as a dependency, or q to cancel."
  (interactive)
  (let ((id (ticket-view--current-id))
        (source-buffer (current-buffer)))
    (ticket-browser-all)
    (with-current-buffer (get-buffer "*tickets*")
      (setq ticket-browser--selection-callback
            (lambda (dep-id)
              (let ((default-directory
                     (with-current-buffer source-buffer
                       (or (ticket--root-directory) default-directory))))
                (shell-command
                 (format "%s dep %s %s" ticket-executable id dep-id))
                (with-current-buffer source-buffer
                  (revert-buffer nil t))
                (message "Added %s as dependency of %s." dep-id id)))))
    (message "Select dependency (RET to confirm, q to cancel).")))

(defun ticket-view-set-parent ()
  "Set the parent of the current ticket via the ticket browser.
Opens the browser in selection mode; press RET on a ticket to set
it as the parent, or q to cancel."
  (interactive)
  (let ((source-buffer (current-buffer)))
    (ticket-browser-all)
    (with-current-buffer (get-buffer "*tickets*")
      (setq ticket-browser--selection-callback
            (lambda (parent-id)
              (with-current-buffer source-buffer
                (ticket-view--set-frontmatter-field "parent" parent-id)
                (save-buffer)
                (message "Parent of %s set to %s."
                         (ticket-view--current-id) parent-id)))))
    (message "Select parent ticket (RET to confirm, q to cancel).")))

(provide 'ticket)
;;; ticket.el ends here
