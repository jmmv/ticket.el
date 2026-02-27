;;; ticket.el --- A simple ticket management tool using `tk`

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
  (let ((repo-script (and load-file-name
                          (expand-file-name "../../repos/ticket/ticket" (file-name-directory load-file-name)))))
    (if (and repo-script (file-executable-p repo-script))
        repo-script
      (executable-find "tk")))
  "The path to the 'ticket' executable.
If 'ticket.el' is loaded from a git repository, this defaults
to the 'ticket' script in the repository. Otherwise, it
defaults to 'tk' which is expected to be in the path."
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
                       (dolist (child-id visible-children)
                         (render-node child-id (1+ depth)))
                     ;; Collapsed: mark entire subtree visited so the fallback
                     ;; loop doesn't surface children at the root level.
                     (dolist (child-id children)
                       (mark-visited child-id)))))))))
      (dolist (root roots)
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
  "Open ticket at point in editor."
  (interactive)
  (let ((id (ticket-browser--ticket-at-point)))
    (when id
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
                (beginning-of-line)))))))))

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

(define-derived-mode ticket-browser-mode special-mode "Tickets"
  "Major mode for browsing tickets as a dependency tree.")

(define-key ticket-browser-mode-map (kbd "RET") 'ticket-browser-open-ticket)
(define-key ticket-browser-mode-map (kbd "TAB") 'ticket-browser-toggle)
(define-key ticket-browser-mode-map (kbd "<") 'ticket-browser-collapse-all)
(define-key ticket-browser-mode-map (kbd ">") 'ticket-browser-expand-all)
(define-key ticket-browser-mode-map (kbd "g") 'ticket-browser-refresh)
(define-key ticket-browser-mode-map (kbd "q") 'quit-window)
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
      "s" s-map)))

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
   ("C" "Close ticket" ticket-close)])

(provide 'ticket)
;;; ticket.el ends here
