;;; test-ticket.el --- Tests for ticket.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'ticket)

(defun ticket-test--write-file (path content)
  "Write CONTENT to PATH, creating parent directories as needed."
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (insert content)))

(defmacro ticket-test--with-temp-project (&rest body)
  "Execute BODY in a temporary project containing a .tickets directory."
  (declare (indent 0) (debug t))
  `(let* ((project-root (make-temp-file "ticket-test-" t))
          (tickets-dir (expand-file-name ".tickets" project-root))
          (default-directory project-root))
     (make-directory tickets-dir t)
     (unwind-protect
         (progn ,@body)
       (delete-directory project-root t))))

(defun ticket-test--buffer-ticket-ids ()
  "Return ticket IDs from current browser buffer in display order."
  (let ((ids '()))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((id (get-text-property (point) 'ticket-browser-id)))
          (when id
            (push id ids)))
        (forward-line 1)))
    (nreverse ids)))

(defun ticket-test--goto-ticket-id (id)
  "Move point to ticket line for ID in the current browser buffer."
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (not (eobp)))
      (if (equal id (get-text-property (point) 'ticket-browser-id))
          (setq found t)
        (forward-line 1)))
    (should found)))

(ert-deftest ticket-test-parse-file-with-full-frontmatter ()
  (ticket-test--with-temp-project
    (let ((file (expand-file-name "abc123.md" tickets-dir)))
      (ticket-test--write-file
       file
       (concat
        "---\n"
        "id: abc123\n"
        "status: in_progress\n"
        "priority: 0\n"
        "type: epic\n"
        "deps: [dep01, dep02]\n"
        "parent: root01\n"
        "---\n"
        "# Implement feature\n"))
      (let ((ticket (ticket-browser--parse-file file)))
        (should ticket)
        (should (equal "abc123" (plist-get ticket :id)))
        (should (equal "in_progress" (plist-get ticket :status)))
        (should (equal "0" (plist-get ticket :priority)))
        (should (equal "epic" (plist-get ticket :type)))
        (should (equal '("dep01" "dep02") (plist-get ticket :deps)))
        (should (equal "root01" (plist-get ticket :parent)))
        (should (equal "Implement feature" (plist-get ticket :title)))))))

(ert-deftest ticket-test-parse-file-populates-defaults ()
  (ticket-test--with-temp-project
    (let ((file (expand-file-name "def456.md" tickets-dir)))
      (ticket-test--write-file
       file
       (concat
        "---\n"
        "id: def456\n"
        "---\n"
        "# Basic ticket\n"))
      (let ((ticket (ticket-browser--parse-file file)))
        (should ticket)
        (should (equal "open" (plist-get ticket :status)))
        (should (equal "2" (plist-get ticket :priority)))
        (should (equal "task" (plist-get ticket :type)))
        (should (equal '() (plist-get ticket :deps)))
        (should (null (plist-get ticket :parent)))
        (should (equal "Basic ticket" (plist-get ticket :title)))))))

(ert-deftest ticket-test-list-tickets-parses-command-output ()
  (cl-letf (((symbol-function 'process-file)
             (lambda (_program _infile destination _display &rest args)
               (should (equal '("list") args))
               (with-current-buffer destination
                 (insert "ID Status Title\n")
                 (insert "abc open First ticket\n")
                 (insert "def in_progress Another one\n"))
               0)))
    (let ((ticket-executable "tk"))
      (should
       (equal '(("abc: First ticket" . "abc")
                ("def: Another one" . "def"))
              (ticket--list-tickets))))))

(ert-deftest ticket-test-build-graph-and-roots ()
  (let* ((tickets (list (list :id "root" :parent nil)
                        (list :id "child" :parent "root")
                        (list :id "orphan" :parent "missing")))
         (graph (ticket-browser--build-graph tickets))
         (id-table (car graph))
         (children-table (cdr graph)))
    (should (equal (plist-get (gethash "root" id-table) :id) "root"))
    (should (equal (gethash "root" children-table) '("child")))
    (should
     (equal '("root" "orphan")
            (ticket-browser--get-roots tickets id-table)))))

(ert-deftest ticket-test-sort-ids-by-priority-prefers-dependencies-as-tiebreaker ()
  (let* ((tickets (list (list :id "x" :priority "1" :deps '("y"))
                        (list :id "y" :priority "1" :deps '())
                        (list :id "z" :priority "0" :deps '())))
         (id-table (car (ticket-browser--build-graph tickets))))
    (should (equal '("z" "y" "x")
                   (ticket-browser--sort-ids-by-priority '("x" "y" "z") id-table)))))

(ert-deftest ticket-test-sort-ids-by-priority-keeps-deterministic-order-for-cycles ()
  (let* ((tickets (list (list :id "a" :priority "1" :deps '("b"))
                        (list :id "b" :priority "1" :deps '("a"))))
         (id-table (car (ticket-browser--build-graph tickets))))
    (should (equal '("a" "b")
                   (ticket-browser--sort-ids-by-priority '("b" "a") id-table)))))

(ert-deftest ticket-test-render-tree-orders-same-priority-roots-by-dependency ()
  (let* ((tickets (list (list :id "x" :status "open" :priority "1" :type "task" :deps '("y") :parent nil :title "X")
                        (list :id "y" :status "open" :priority "1" :type "task" :deps '() :parent nil :title "Y")))
         (graph (ticket-browser--build-graph tickets)))
    (with-temp-buffer
      (setq-local ticket-browser--expanded (make-hash-table :test 'equal))
      (ticket-browser--render-tree tickets graph 'all)
      (should (equal '("y" "x") (ticket-test--buffer-ticket-ids))))))

(ert-deftest ticket-test-render-tree-orders-same-priority-siblings-by-dependency ()
  (let* ((tickets (list (list :id "p" :status "open" :priority "0" :type "epic" :deps '() :parent nil :title "Parent")
                        (list :id "x" :status "open" :priority "1" :type "task" :deps '("y") :parent "p" :title "X")
                        (list :id "y" :status "open" :priority "1" :type "task" :deps '() :parent "p" :title "Y")))
         (graph (ticket-browser--build-graph tickets)))
    (with-temp-buffer
      (setq-local ticket-browser--expanded (make-hash-table :test 'equal))
      (puthash "p" t ticket-browser--expanded)
      (ticket-browser--render-tree tickets graph 'all)
      (should (equal '("p" "y" "x") (ticket-test--buffer-ticket-ids))))))

(ert-deftest ticket-test-render-tree-respects-filter ()
  (let* ((tickets (list (list :id "a" :status "open" :priority "1" :type "task" :parent nil :title "Root")
                        (list :id "b" :status "closed" :priority "2" :type "task" :parent "a" :title "Closed child")
                        (list :id "c" :status "open" :priority "0" :type "bug" :parent "a" :title "Open child")))
         (graph (ticket-browser--build-graph tickets)))
    (with-temp-buffer
      (setq-local ticket-browser--expanded (make-hash-table :test 'equal))
      (puthash "a" t ticket-browser--expanded)
      (ticket-browser--render-tree tickets graph 'open-only)
      (should (string-match-p "\\ba\\b" (buffer-string)))
      (should (string-match-p "\\bc\\b" (buffer-string)))
      (should-not (string-match-p "\\bb\\b" (buffer-string)))
      (goto-char (point-min))
      (should (equal "a" (get-text-property (point) 'ticket-browser-id)))))
  (let* ((tickets (list (list :id "a" :status "open" :priority "1" :type "task" :parent nil :title "Root")
                        (list :id "b" :status "closed" :priority "2" :type "task" :parent "a" :title "Closed child")))
         (graph (ticket-browser--build-graph tickets)))
    (with-temp-buffer
      (setq-local ticket-browser--expanded (make-hash-table :test 'equal))
      (puthash "a" t ticket-browser--expanded)
      (ticket-browser--render-tree tickets graph 'all)
      (should (string-match-p "\\bb\\b" (buffer-string))))))

(ert-deftest ticket-test-browser-expand-cycle-expands-current-then-all ()
  (let ((ticket-browser--tickets
         (list (list :id "a" :status "open" :priority "1" :type "task" :parent nil :title "A")
               (list :id "a1" :status "open" :priority "1" :type "task" :parent "a" :title "A1")
               (list :id "c" :status "open" :priority "1" :type "task" :parent nil :title "C"))))
    (with-temp-buffer
      (ticket-browser-mode)
      (setq-local ticket-browser--expanded (make-hash-table :test 'equal))
      (setq-local ticket-browser--filter 'all)
      (ticket-browser--redisplay)
      (ticket-test--goto-ticket-id "a")
      (ticket-browser-expand-cycle)
      (should (eq t (gethash "a" ticket-browser--expanded)))
      (should-not (gethash "a1" ticket-browser--expanded))
      (should-not (gethash "c" ticket-browser--expanded))
      (ticket-test--goto-ticket-id "a")
      (ticket-browser-expand-cycle)
      (dolist (ticket ticket-browser--tickets)
        (should (eq t (gethash (plist-get ticket :id) ticket-browser--expanded)))))))

(ert-deftest ticket-test-browser-collapse-cycle-collapses-others-then-current ()
  (let ((ticket-browser--tickets
         (list (list :id "a" :status "open" :priority "1" :type "task" :parent nil :title "A")
               (list :id "a1" :status "open" :priority "1" :type "task" :parent "a" :title "A1")
               (list :id "c" :status "open" :priority "1" :type "task" :parent nil :title "C"))))
    (with-temp-buffer
      (ticket-browser-mode)
      (setq-local ticket-browser--expanded (make-hash-table :test 'equal))
      (setq-local ticket-browser--filter 'all)
      (dolist (ticket ticket-browser--tickets)
        (puthash (plist-get ticket :id) t ticket-browser--expanded))
      (ticket-browser--redisplay)
      (ticket-test--goto-ticket-id "a")
      (ticket-browser-collapse-cycle)
      (should (eq t (gethash "a" ticket-browser--expanded)))
      (should-not (gethash "a1" ticket-browser--expanded))
      (should-not (gethash "c" ticket-browser--expanded))
      (ticket-test--goto-ticket-id "a")
      (ticket-browser-collapse-cycle)
      (should-not (gethash "a" ticket-browser--expanded)))))

(ert-deftest ticket-test-browser-expand-cycle-resets-when-selection-changes ()
  (let ((ticket-browser--tickets
         (list (list :id "a" :status "open" :priority "1" :type "task" :parent nil :title "A")
               (list :id "a1" :status "open" :priority "1" :type "task" :parent "a" :title "A1")
               (list :id "c" :status "open" :priority "1" :type "task" :parent nil :title "C")
               (list :id "c1" :status "open" :priority "1" :type "task" :parent "c" :title "C1"))))
    (with-temp-buffer
      (ticket-browser-mode)
      (setq-local ticket-browser--expanded (make-hash-table :test 'equal))
      (setq-local ticket-browser--filter 'all)
      (ticket-browser--redisplay)
      (ticket-test--goto-ticket-id "a")
      (ticket-browser-expand-cycle)
      (ticket-test--goto-ticket-id "c")
      (ticket-browser-expand-cycle)
      (should (eq t (gethash "a" ticket-browser--expanded)))
      (should (eq t (gethash "c" ticket-browser--expanded)))
      (should-not (gethash "a1" ticket-browser--expanded))
      (should-not (gethash "c1" ticket-browser--expanded)))))

(ert-deftest ticket-test-set-frontmatter-field-rewrites-existing-entry ()
  (with-temp-buffer
    (insert
     (concat
      "---\n"
      "id: abc\n"
      "type: task\n"
      "---\n"
      "# Title\n"))
    (ticket-view--set-frontmatter-field "type" "epic")
    (should
     (string-match-p "^type: epic$" (buffer-string)))))

(ert-deftest ticket-test-set-frontmatter-field-inserts-missing-entry ()
  (with-temp-buffer
    (insert
     (concat
      "---\n"
      "id: abc\n"
      "---\n"
      "# Title\n"))
    (ticket-view--set-frontmatter-field "parent" "root")
    (should (string-match-p "^parent: root$" (buffer-string)))))

(ert-deftest ticket-test-view-set-status-closed-issues-command ()
  (ticket-test--with-temp-project
    (let* ((file (expand-file-name "abc.md" tickets-dir))
           (recorded-program nil)
           (recorded-args nil)
           (reverted nil)
           (ticket-executable "tk"))
      (ticket-test--write-file
       file
       (concat
        "---\n"
        "id: abc\n"
        "status: open\n"
        "---\n"
        "# Example\n"))
      (with-current-buffer (find-file-noselect file)
        (unwind-protect
            (cl-letf (((symbol-function 'process-file)
                       (lambda (program _infile _destination _display &rest args)
                         (setq recorded-program program)
                         (setq recorded-args args)
                         0))
                      ((symbol-function 'revert-buffer)
                       (lambda (&rest _)
                         (setq reverted t))))
              (ticket-view-set-status-closed))
          (set-buffer-modified-p nil)
          (kill-buffer (current-buffer))))
      (should (equal "tk" recorded-program))
      (should (equal '("close" "abc") recorded-args))
      (should reverted))))

(ert-deftest ticket-test-browser-open-ticket-restores-browser-on-kill ()
  (ticket-test--with-temp-project
    (let* ((file (expand-file-name "abc.md" tickets-dir))
           (browser-buffer (get-buffer-create "*tickets-test*")))
      (ticket-test--write-file
       file
       (concat
        "---\n"
        "id: abc\n"
        "status: open\n"
        "---\n"
        "# Example\n"))
      (unwind-protect
          (save-window-excursion
            (with-current-buffer browser-buffer
              (ticket-browser-mode)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert "abc  [P2] task  Example\n")
                (goto-char (point-min))
                (put-text-property (line-beginning-position)
                                   (line-end-position)
                                   'ticket-browser-id "abc"))
              (goto-char (point-min)))
            (switch-to-buffer browser-buffer)
            (ticket-browser-open-ticket)
            (let ((opened-buffer (current-buffer)))
              (should (equal (buffer-file-name opened-buffer) file))
              (kill-buffer opened-buffer)
              (sleep-for 0.01)
              (should (eq (window-buffer (selected-window)) browser-buffer))))
        (when (buffer-live-p browser-buffer)
          (kill-buffer browser-buffer))))))

(ert-deftest ticket-test-browser-open-ticket-no-restore-when-disabled ()
  (ticket-test--with-temp-project
    (let* ((file (expand-file-name "abc.md" tickets-dir))
           (browser-buffer (get-buffer-create "*tickets-test*"))
           (ticket-browser-restore-on-ticket-close nil))
      (ticket-test--write-file
       file
       (concat
        "---\n"
        "id: abc\n"
        "status: open\n"
        "---\n"
        "# Example\n"))
      (unwind-protect
          (save-window-excursion
            (with-current-buffer browser-buffer
              (ticket-browser-mode)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert "abc  [P2] task  Example\n")
                (goto-char (point-min))
                (put-text-property (line-beginning-position)
                                   (line-end-position)
                                   'ticket-browser-id "abc"))
              (goto-char (point-min)))
            (switch-to-buffer browser-buffer)
            (ticket-browser-open-ticket)
            (let ((opened-buffer (current-buffer)))
              (should (equal (buffer-file-name opened-buffer) file))
              (should-not (local-variable-p 'ticket-view--return-buffer opened-buffer))
              (should-not (local-variable-p 'ticket-view--return-window opened-buffer))
              (should-not (local-variable-p 'kill-buffer-hook opened-buffer))
              (kill-buffer opened-buffer)))
        (when (buffer-live-p browser-buffer)
          (kill-buffer browser-buffer))))))

(ert-deftest ticket-test-view-open-ticket-at-point ()
  (ticket-test--with-temp-project
    (let* ((source-file (expand-file-name "abc.md" tickets-dir))
           (target-file (expand-file-name "dep-01.md" tickets-dir)))
      (ticket-test--write-file
       source-file
       (concat
        "---\n"
        "id: abc\n"
        "deps: [dep-01]\n"
        "---\n"
        "# Example\n"))
      (ticket-test--write-file
       target-file
       (concat
        "---\n"
        "id: dep-01\n"
        "---\n"
        "# Dependency\n"))
      (with-current-buffer (find-file-noselect source-file)
        (unwind-protect
            (save-window-excursion
              (switch-to-buffer (current-buffer))
              (goto-char (point-min))
              (search-forward "dep-01")
              (backward-char 2)
              (ticket-view-open-ticket-at-point)
              (should (equal (buffer-file-name (current-buffer)) target-file)))
          (let ((source-buffer (get-file-buffer source-file))
                (target-buffer (get-file-buffer target-file)))
            (when (buffer-live-p source-buffer)
              (with-current-buffer source-buffer
                (set-buffer-modified-p nil))
              (kill-buffer source-buffer))
            (when (buffer-live-p target-buffer)
              (with-current-buffer target-buffer
                (set-buffer-modified-p nil))
              (kill-buffer target-buffer))))))))

(ert-deftest ticket-test-view-open-ticket-at-point-errors-for-missing-ticket ()
  (ticket-test--with-temp-project
    (let ((source-file (expand-file-name "abc.md" tickets-dir)))
      (ticket-test--write-file
       source-file
       (concat
        "---\n"
        "id: abc\n"
        "parent: missing\n"
        "---\n"
        "# Example\n"))
      (with-current-buffer (find-file-noselect source-file)
        (unwind-protect
            (progn
              (goto-char (point-min))
              (search-forward "missing")
              (backward-char 1)
              (should-error (ticket-view-open-ticket-at-point)))
          (set-buffer-modified-p nil)
          (kill-buffer (current-buffer)))))))

(ert-deftest ticket-test-transient-uses-browse-keybinding ()
  (let* ((suffix (transient-get-suffix 'ticket-transient "b"))
         (spec (caddr suffix)))
    (should (equal (plist-get spec :description) "Browse tickets"))
    (should (eq (plist-get spec :command) 'ticket-browser))
    (should-error (transient-get-suffix 'ticket-transient "l"))
    (should-error (transient-get-suffix 'ticket-transient "L"))))

(ert-deftest ticket-test-transient-includes-open-ticket-at-point ()
  (let* ((ticket-view-mode t)
         (suffix (transient-get-suffix 'ticket-transient "o"))
         (spec (caddr suffix)))
    (should (equal (plist-get spec :description) "Open ticket at point"))
    (should (eq (plist-get spec :command) 'ticket-view-open-ticket-at-point))))

(ert-deftest ticket-test-view-mode-map-binds-open-ticket-at-point ()
  (should (eq (lookup-key ticket-view-mode-map (kbd "C-c k o"))
              #'ticket-view-open-ticket-at-point)))

(ert-deftest ticket-test-browser-mode-map-binds-priority-and-tree-shortcuts ()
  (should (eq (lookup-key ticket-browser-mode-map (kbd "<"))
              #'ticket-browser-increase-priority))
  (should (eq (lookup-key ticket-browser-mode-map (kbd ">"))
              #'ticket-browser-decrease-priority))
  (should (eq (lookup-key ticket-browser-mode-map (kbd "TAB"))
              #'ticket-browser-expand-cycle))
  (should (eq (lookup-key ticket-browser-mode-map (kbd "<backtab>"))
              #'ticket-browser-collapse-cycle))
  (should (eq (lookup-key ticket-browser-mode-map (kbd "q"))
              #'ticket-browser-quit))
  (should (eq (lookup-key ticket-browser-mode-map (kbd "?"))
              #'ticket-browser-menu))
  (should (eq (lookup-key ticket-browser-mode-map (kbd "c"))
              #'ticket-browser-close-selected-ticket))
  (should (eq (lookup-key ticket-browser-mode-map (kbd "o"))
              #'ticket-browser-reopen-selected-ticket))
  (should (eq (lookup-key ticket-browser-mode-map (kbd "d"))
              #'ticket-browser-set-dep-for-selected-ticket))
  (should (eq (lookup-key ticket-browser-mode-map (kbd "p"))
              #'ticket-browser-set-parent-for-selected-ticket))
  (should-not (lookup-key ticket-browser-mode-map (kbd "[")))
  (should-not (lookup-key ticket-browser-mode-map (kbd "]"))))

(ert-deftest ticket-test-browser-transient-includes-edit-actions ()
  (let* ((close-suffix (transient-get-suffix 'ticket-browser-transient "c"))
         (close-spec (caddr close-suffix))
         (dep-suffix (transient-get-suffix 'ticket-browser-transient "d"))
         (dep-spec (caddr dep-suffix)))
    (should (equal (plist-get close-spec :description) "Close selected ticket"))
    (should (eq (plist-get close-spec :command)
                'ticket-browser-close-selected-ticket))
    (should (equal (plist-get dep-spec :description) "Add dependency to selected"))
    (should (eq (plist-get dep-spec :command)
                'ticket-browser-set-dep-for-selected-ticket))))

(ert-deftest ticket-test-browser-quit-clears-selection-callback ()
  (with-temp-buffer
    (ticket-browser-mode)
    (setq ticket-browser--selection-callback (lambda (_id) nil))
    (let ((quit-called nil))
      (cl-letf (((symbol-function 'quit-window)
                 (lambda (&rest _) (setq quit-called t))))
        (ticket-browser-quit))
      (should quit-called)
      (should-not ticket-browser--selection-callback))))

(ert-deftest ticket-test-browser-close-selected-ticket-runs-close-and-refreshes ()
  (with-temp-buffer
    (ticket-browser-mode)
    (let ((inhibit-read-only t))
      (insert (propertize "abc\n" 'ticket-browser-id "abc")))
    (goto-char (point-min))
    (let ((tk-args nil)
          (refreshed nil))
      (cl-letf (((symbol-function 'ticket--run-tk-checked)
                 (lambda (&rest args) (setq tk-args args)))
                ((symbol-function 'ticket-browser-refresh)
                 (lambda () (setq refreshed t))))
        (ticket-browser-close-selected-ticket))
      (should (equal '("close" "abc") tk-args))
      (should refreshed))))

(ert-deftest ticket-test-browser-reopen-selected-ticket-runs-reopen-and-refreshes ()
  (with-temp-buffer
    (ticket-browser-mode)
    (let ((inhibit-read-only t))
      (insert (propertize "abc\n" 'ticket-browser-id "abc")))
    (goto-char (point-min))
    (let ((tk-args nil)
          (refreshed nil))
      (cl-letf (((symbol-function 'ticket--run-tk-checked)
                 (lambda (&rest args) (setq tk-args args)))
                ((symbol-function 'ticket-browser-refresh)
                 (lambda () (setq refreshed t))))
        (ticket-browser-reopen-selected-ticket))
      (should (equal '("reopen" "abc") tk-args))
      (should refreshed))))

(ert-deftest ticket-test-browser-set-dep-for-selected-ticket-selection-callback-runs-command ()
  (let ((tk-args nil)
        (refreshed nil)
        (callback nil))
    (cl-letf (((symbol-function 'ticket-browser-all)
               (lambda ()
                 (with-current-buffer (get-buffer-create "*tickets*")
                   (ticket-browser-mode))))
              ((symbol-function 'ticket--run-tk-checked)
               (lambda (&rest args) (setq tk-args args)))
              ((symbol-function 'ticket-browser-refresh)
               (lambda () (setq refreshed t))))
      (with-temp-buffer
        (ticket-browser-mode)
        (let ((inhibit-read-only t))
          (insert (propertize "abc\n" 'ticket-browser-id "abc")))
        (goto-char (point-min))
        (ticket-browser-set-dep-for-selected-ticket))
      (with-current-buffer (get-buffer "*tickets*")
        (setq callback ticket-browser--selection-callback))
      (funcall callback "dep-01")
      (should (equal '("dep" "abc" "dep-01") tk-args))
      (should refreshed))))

(ert-deftest ticket-test-browser-set-parent-for-selected-ticket-selection-callback-updates-frontmatter ()
  (let ((set-args nil)
        (refreshed nil)
        (callback nil))
    (cl-letf (((symbol-function 'ticket-browser-all)
               (lambda ()
                 (with-current-buffer (get-buffer-create "*tickets*")
                   (ticket-browser-mode))))
              ((symbol-function 'ticket-browser--set-ticket-frontmatter-field)
               (lambda (&rest args) (setq set-args args)))
              ((symbol-function 'ticket-browser-refresh)
               (lambda () (setq refreshed t))))
      (with-temp-buffer
        (ticket-browser-mode)
        (let ((inhibit-read-only t))
          (insert (propertize "abc\n" 'ticket-browser-id "abc")))
        (goto-char (point-min))
        (ticket-browser-set-parent-for-selected-ticket))
      (with-current-buffer (get-buffer "*tickets*")
        (setq callback ticket-browser--selection-callback))
      (funcall callback "root-01")
      (should (equal '("abc" "parent" "root-01") set-args))
      (should refreshed))))

(ert-deftest ticket-test-browser-close-selected-ticket-uses-transient-source-buffer ()
  (let ((source-buffer (generate-new-buffer "*tickets-source*"))
        (tk-args nil)
        (refreshed nil))
    (unwind-protect
        (progn
          (with-current-buffer source-buffer
            (ticket-browser-mode)
            (let ((inhibit-read-only t))
              (insert (propertize "abc\n" 'ticket-browser-id "abc")))
            (goto-char (point-max)))
          (with-temp-buffer
            (setq ticket-browser--transient-source-buffer source-buffer)
            (cl-letf (((symbol-function 'ticket--run-tk-checked)
                       (lambda (&rest args) (setq tk-args args)))
                      ((symbol-function 'ticket-browser-refresh)
                       (lambda () (setq refreshed t))))
              (ticket-browser-close-selected-ticket))))
      (kill-buffer source-buffer))
    (should (equal '("close" "abc") tk-args))
    (should refreshed)))

;;; test-ticket.el ends here
