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
        "parent: root01\n"
        "---\n"
        "# Implement feature\n"))
      (let ((ticket (ticket-browser--parse-file file)))
        (should ticket)
        (should (equal "abc123" (plist-get ticket :id)))
        (should (equal "in_progress" (plist-get ticket :status)))
        (should (equal "0" (plist-get ticket :priority)))
        (should (equal "epic" (plist-get ticket :type)))
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
        (should (null (plist-get ticket :parent)))
        (should (equal "Basic ticket" (plist-get ticket :title)))))))

(ert-deftest ticket-test-list-tickets-parses-command-output ()
  (cl-letf (((symbol-function 'shell-command)
             (lambda (_command output-buffer &rest _)
               (with-current-buffer output-buffer
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

(ert-deftest ticket-test-view-set-status-closed-issues-command ()
  (ticket-test--with-temp-project
    (let* ((file (expand-file-name "abc.md" tickets-dir))
           (recorded-command nil)
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
            (cl-letf (((symbol-function 'shell-command)
                       (lambda (command &rest _)
                         (setq recorded-command command)
                         0))
                      ((symbol-function 'revert-buffer)
                       (lambda (&rest _)
                         (setq reverted t))))
              (ticket-view-set-status-closed))
          (set-buffer-modified-p nil)
          (kill-buffer (current-buffer))))
      (should (equal "tk close abc" recorded-command))
      (should reverted))))

;;; test-ticket.el ends here
