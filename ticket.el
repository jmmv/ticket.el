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

(defcustom ticket-browser-restore-on-ticket-close t
  "When non-nil, closing a ticket opened from the browser restores it.
If enabled, opening a ticket from `*tickets*' records that browser
buffer/window and killing the ticket buffer returns the same window to
the browser."
  :type 'boolean
  :group 'ticket)

(defun ticket--run-tk (&rest args)
  "Run `tk' with ARGS and return its exit code.
Signals an error if `ticket-executable' is not configured."
  (let ((default-directory (or (ticket--root-directory) default-directory)))
    (unless ticket-executable
      (user-error "ticket-executable is not configured"))
    (apply #'process-file ticket-executable nil nil nil args)))

(defun ticket--run-tk-checked (&rest args)
  "Run `tk' with ARGS and signal an error on non-zero exit."
  (let ((status (apply #'ticket--run-tk args)))
    (unless (eq status 0)
      (error "tk command failed (%s): %s" status (string-join args " ")))
    status))

(defun ticket--run-tk-to-string (&rest args)
  "Run `tk' with ARGS and return stdout as a trimmed string."
  (with-temp-buffer
    (let ((status (apply #'ticket--run-tk-to-buffer (current-buffer) args)))
      (unless (eq status 0)
        (error "tk command failed (%s): %s" status (string-join args " ")))
      (string-trim (buffer-string)))))

(defun ticket--run-tk-to-buffer (buffer &rest args)
  "Run `tk' with ARGS, sending stdout to BUFFER."
  (let ((default-directory (or (ticket--root-directory) default-directory)))
    (unless ticket-executable
      (user-error "ticket-executable is not configured"))
    (apply #'process-file ticket-executable nil buffer nil args)))

(defun ticket--frontmatter-bounds ()
  "Return the frontmatter bounds as (START . END), or nil."
  (save-excursion
    (goto-char (point-min))
    (when (looking-at-p "^---[ \t]*$")
      (forward-line)
      (let ((fm-start (point)))
        (when (re-search-forward "^---[ \t]*$" nil t)
          (cons fm-start (match-beginning 0)))))))

(defun ticket--set-frontmatter-field (field value &optional insert-if-missing)
  "Set FIELD to VALUE in current buffer's frontmatter.
When INSERT-IF-MISSING is non-nil, append FIELD before the closing
frontmatter delimiter if the field is missing.
Returns non-nil if the field was written."
  (let ((bounds (ticket--frontmatter-bounds))
        (updated nil))
    (when bounds
      (save-excursion
        (goto-char (car bounds))
        (if (re-search-forward
             (format "^%s:[ \t]*.*$" (regexp-quote field))
             (cdr bounds) t)
            (progn
              (replace-match (format "%s: %s" field value))
              (setq updated t))
          (when insert-if-missing
            (goto-char (cdr bounds))
            (insert (format "%s: %s\n" field value))
            (setq updated t)))))
    updated))

(defun ticket--ticket-file (id)
  "Return full path to ticket ID's file, or nil if no ticket dir."
  (let ((dir (ticket-directory)))
    (when dir
      (expand-file-name (concat id ".md") dir))))

(defun ticket--list-tickets ()
  "Return a list of open tickets."
  (with-temp-buffer
    (let ((status (ticket--run-tk-to-buffer (current-buffer) "list")))
      (unless (eq status 0)
        (error "tk command failed (%s): list" status)))
    (goto-char (point-min))
    (let ((tickets ()))
      (while (not (eobp))
        (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
               (parts (split-string line " " t))
               (id (car parts))
               (title (string-join (cddr parts) " ")))
          (unless (or (null id) (string-match-p "^ID\\'" id))
            (push (cons (format "%s: %s" id title) id) tickets)))
        (forward-line))
      (nreverse tickets))))

;;;###autoload
(defun ticket-create (title)
  "Create a new ticket with TITLE and open it for editing."
  (interactive "sTicket title: ")
  (let* ((ticket-id (ticket--run-tk-to-string "create" title))
         (ticket-file (ticket--ticket-file ticket-id)))
    (unless ticket-file
      (user-error "Cannot locate .tickets directory"))
    (find-file ticket-file)
    (goto-char (point-max))))

;;;###autoload
(defun ticket-close ()
  "Select a ticket to close."
  (interactive)
  (let* ((tickets (ticket--list-tickets))
         (choice (completing-read "Close ticket: " tickets nil t))
         (ticket-id (cdr (assoc choice tickets))))
    (when ticket-id
      (ticket--run-tk-checked "close" ticket-id)
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

(defvar-local ticket-view--return-buffer nil
  "Browser buffer to restore when this ticket buffer is closed.")

(defvar-local ticket-view--return-window nil
  "Window where the browser buffer should be restored.")

(defun ticket-view--restore-browser-on-kill ()
  "Restore the originating ticket browser in its window when closing."
  (when (and ticket-view--return-window
             (window-live-p ticket-view--return-window)
             ticket-view--return-buffer
             (buffer-live-p ticket-view--return-buffer))
    ;; Run after kill-buffer finishes so other buffer replacement logic
    ;; (e.g. framework-specific kill commands) doesn't override this.
    (run-at-time
     0 nil
     (lambda (window buffer)
       (when (and (window-live-p window)
                  (buffer-live-p buffer))
         (set-window-buffer window buffer)))
     ticket-view--return-window
     ticket-view--return-buffer)))

(defun ticket-browser--parse-id-list (value)
  "Parse VALUE as a list of ticket ids.
Understands tk's inline YAML list format like \"[a, b]\"."
  (let* ((trimmed (string-trim (or value "")))
         (content (if (and (string-prefix-p "[" trimmed)
                           (string-suffix-p "]" trimmed))
                      (substring trimmed 1 -1)
                    trimmed)))
    (if (string-empty-p (string-trim content))
        '()
      (seq-filter (lambda (id) (not (string-empty-p id)))
                  (mapcar #'string-trim (split-string content "," t))))))

(defun ticket-browser--ticket-priority (id id-table)
  "Return numeric priority for ID from ID-TABLE."
  (let ((ticket (gethash id id-table)))
    (if ticket
        (string-to-number (or (plist-get ticket :priority) "2"))
      2)))

(defun ticket-browser--sort-ids-by-dependency (ids id-table)
  "Sort IDS so dependencies appear before dependents when possible.
Only dependencies within IDS are considered.  Cycles or ambiguous nodes
fall back to lexicographic id order."
  (let ((id-set (make-hash-table :test 'equal))
        (in-degree (make-hash-table :test 'equal))
        (edges (make-hash-table :test 'equal))
        (seen (make-hash-table :test 'equal))
        (ready '())
        (ordered '()))
    (dolist (id ids)
      (puthash id t id-set)
      (puthash id 0 in-degree)
      (puthash id '() edges))
    (dolist (id ids)
      (let ((ticket (gethash id id-table)))
        (dolist (dep (or (plist-get ticket :deps) '()))
          (when (and (not (equal dep id))
                     (gethash dep id-set)
                     (not (member id (gethash dep edges '()))))
            (puthash dep (cons id (gethash dep edges '())) edges)
            (puthash id (1+ (gethash id in-degree 0)) in-degree)))))
    (maphash (lambda (id degree)
               (when (zerop degree)
                 (push id ready)))
             in-degree)
    (setq ready (sort ready #'string<))
    (while ready
      (let ((id (pop ready)))
        (unless (gethash id seen)
          (puthash id t seen)
          (push id ordered)
          (dolist (dependent (gethash id edges '()))
            (let ((degree (1- (gethash dependent in-degree 0))))
              (puthash dependent degree in-degree)
              (when (zerop degree)
                (push dependent ready))))
          (setq ready (sort ready #'string<)))))
    (setq ordered (nreverse ordered))
    (let ((remaining '()))
      (dolist (id ids)
        (unless (gethash id seen)
          (push id remaining)))
      (append ordered (sort remaining #'string<)))))

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
                (id nil) (status nil) (priority nil) (type nil) (parent nil) (deps nil))
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
               ((string-match "^deps:[ \t]*\\(.*\\)$" line)
                (setq deps (ticket-browser--parse-id-list (match-string 1 line))))
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
                      :deps (or deps '())
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
  "Sort IDS by priority and dependencies.
Lower priority numbers appear first.  IDs with equal priority are then
ordered so dependencies appear before dependents when possible."
  (let ((groups (make-hash-table :test 'eql))
        (priorities '()))
    (dolist (id ids)
      (let ((priority (ticket-browser--ticket-priority id id-table)))
        (unless (gethash priority groups)
          (push priority priorities))
        (puthash priority (cons id (gethash priority groups '())) groups)))
    (setq priorities (sort priorities #'<))
    (let ((result '()))
      (dolist (priority priorities)
        (let* ((group (nreverse (gethash priority groups '())))
               (sorted (ticket-browser--sort-ids-by-dependency group id-table)))
          (setq result (append result sorted))))
      result)))

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
        (let ((browser-buffer (current-buffer))
              (browser-window (selected-window))
              (browser-window-point (point))
              (browser-window-start (window-start))
              (file (expand-file-name (concat id ".md") (ticket-directory))))
          (when (file-exists-p file)
            (find-file file)
            (when ticket-browser-restore-on-ticket-close
              ;; Seed previous-buffer history so generic kill-buffer flows
              ;; naturally switch this window back to the browser.
              (set-window-prev-buffers
               browser-window
               (cons (list browser-buffer
                           (copy-marker browser-window-start)
                           (copy-marker browser-window-point))
                     (seq-remove (lambda (entry)
                                   (eq (car entry) browser-buffer))
                                 (window-prev-buffers browser-window))))
              (setq-local ticket-view--return-buffer browser-buffer)
              (setq-local ticket-view--return-window browser-window)
              (add-hook 'kill-buffer-hook #'ticket-view--restore-browser-on-kill nil t))
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
  (let ((file (ticket--ticket-file id)))
    (when (and file (file-exists-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (ticket--set-frontmatter-field "priority"
                                       (number-to-string new-priority)
                                       t)
        (write-region (point-min) (point-max) file nil 'silent)))))

(defun ticket-browser--ticket-by-id (id)
  "Return loaded ticket plist for ID, or nil."
  (seq-find (lambda (ticket) (equal (plist-get ticket :id) id))
            ticket-browser--tickets))

(defun ticket-browser--adjust-priority (delta &optional min-priority)
  "Adjust priority of ticket at point by DELTA.
When MIN-PRIORITY is non-nil, clamp to that minimum."
  (let ((id (ticket-browser--ticket-at-point)))
    (when id
      (let* ((ticket (ticket-browser--ticket-by-id id))
             (current (string-to-number (or (plist-get ticket :priority) "2")))
             (candidate (+ current delta))
             (new-priority (if min-priority
                               (max min-priority candidate)
                             candidate)))
        (ticket-browser--set-priority id new-priority)
        (ticket-browser-refresh)
        (message "Ticket %s priority: P%d" id new-priority)))))

;;;###autoload
(defun ticket-browser-increase-priority ()
  "Increase priority of ticket at point (decrease number, min P0)."
  (interactive)
  (ticket-browser--adjust-priority -1 0))

;;;###autoload
(defun ticket-browser-decrease-priority ()
  "Decrease priority of ticket at point (increase number, no limit)."
  (interactive)
  (ticket-browser--adjust-priority 1))

(defun ticket-browser--make-filter-map ()
  "Build the keymap for browser filter commands."
  (let ((map (make-sparse-keymap)))
    (define-key map "o" 'ticket-browser-show-open-only)
    (define-key map "a" 'ticket-browser-show-all)
    map))

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
(define-key ticket-browser-mode-map "s" (ticket-browser--make-filter-map))
;; evil-define-key is a macro and can't be called safely at byte-compile time;
;; use evil-define-key* (the underlying function) inside with-eval-after-load.
(with-eval-after-load 'evil
  (let ((s-map (ticket-browser--make-filter-map)))
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
   ("b" "Browse tickets" ticket-browser)
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
  (ticket--set-frontmatter-field field value t))

(defun ticket-view--set-status (verb command)
  "Run ticket status COMMAND and report VERB for current ticket."
  (let ((id (ticket-view--current-id)))
    (ticket--run-tk-checked command id)
    (revert-buffer nil t)
    (message "Ticket %s %s." id verb)))

(defun ticket-view-set-status-closed ()
  "Close the current ticket."
  (interactive)
  (ticket-view--set-status "closed" "close"))

(defun ticket-view-set-status-open ()
  "Reopen the current ticket."
  (interactive)
  (ticket-view--set-status "reopened" "reopen"))

(defun ticket-view--set-type (type)
  "Set the current ticket's type field to TYPE."
  (ticket-view--set-frontmatter-field "type" type)
  (save-buffer)
  (message "Type set to '%s'." type))

(defun ticket-view-set-type-task ()
  "Set the current ticket's type to 'task'."
  (interactive)
  (ticket-view--set-type "task"))

(defun ticket-view-set-type-epic ()
  "Set the current ticket's type to 'epic'."
  (interactive)
  (ticket-view--set-type "epic"))

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
                (ticket--run-tk-checked "dep" id dep-id)
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
