(defvar mode-supports-multi-select-alist
  '((go-mode . t)
    (go-ts-mode . t)
    (python-mode . t)
    (python-ts-mode . t))
  "Association list mapping major modes to whether they support multiple test selection.
Only modes listed here with a value of t can use select-current-test-at-point.")

(defun tap--mode-supports-multi-select-p ()
  "Check if the current major mode supports multiple test selection."
  (cdr (assoc major-mode mode-supports-multi-select-alist)))

;;;###autoload
(defun select-current-test-at-point ()
  "Adds the result of `current-test-at-point' to the *test-at-point-selections* buffer.
Only works for languages that support running multiple tests (currently Go and Python)."
  (interactive)
  (if (not (tap--mode-supports-multi-select-p))
      (message "Multi-test selection not supported for %s mode. Only Go and Python currently support this feature." major-mode)
    (let ((test-identifier (current-test-at-point)))
      (message (prin1-to-string (type-of test-identifier)))
      (message (concat "file: " (car test-identifier)))
      (message (concat "test: " (cdr test-identifier)))
      (with-current-buffer (get-buffer-create "*test-at-point-selections*")
        (goto-char (point-max))
        (message (concat "Adding test: " (cdr test-identifier)))
        (insert (tap--make-test-string test-identifier))
        (insert "\n")))))


;;;###autoload
(defun remove-current-test-at-point-from-buffer ()
  "Removes the result of `current-test-at-point' from the *test-at-point-selections* buffer.
Only works for languages that support running multiple tests (currently Go and Python)."
  (interactive)
  (if (not (tap--mode-supports-multi-select-p))
      (message "Multi-test selection not supported for %s mode. Only Go and Python currently support this feature." major-mode)
    (let ((test-string (tap--make-test-string (current-test-at-point))))
      (with-current-buffer (get-buffer "*test-at-point-selections*")
        (when (buffer-live-p (current-buffer))
          (goto-char (point-min))
          (while (not (eobp))
            (let ((line-start (point)))
              (forward-line 1)
              (let* ((line-end (point)) ; Define line-end here
                     (line (string-trim (buffer-substring-no-properties line-start line-end))))
                (when (string= line test-string)
                  (delete-region line-start line-end)
                  (goto-char (point-min)) ; Restart search from beginning
                  (forward-line 0)))))))))) ; Ensure forward-line doesn't move unnecessarily

;;;###autoload
(defun test-at-point-show-selected ()
  "Display the *test-at-point-selections* buffer showing all selected tests."
  (interactive)
  (if (not (get-buffer "*test-at-point-selections*"))
      (message "No tests selected yet. Use select-current-test-at-point to add tests.")
    (pop-to-buffer "*test-at-point-selections*")))

;;;###autoload
(defun test-at-point-clear-selected ()
  "Clear all selected tests from the *test-at-point-selections* buffer."
  (interactive)
  (if (not (get-buffer "*test-at-point-selections*"))
      (message "No tests to clear.")
    (with-current-buffer "*test-at-point-selections*"
      (erase-buffer)
      (message "Cleared all selected tests."))))

;;;###autoload
(defun test-at-point-run-selected ()
  "Run all tests that have been added to the *test-at-point-selections* buffer.
Only works for languages that support running multiple tests (currently Go and Python)."
  (interactive)
  (if (not (tap--mode-supports-multi-select-p))
      (message "Multi-test selection not supported for %s mode. Only Go and Python currently support this feature." major-mode)
    (let* ((tests (with-current-buffer "*test-at-point-selections*"
                    (buffer-lines-as-list)))
           (mode-command (cdr (assoc major-mode mode-command-pattern-alist)))
           (project-overides (cdr (assoc (projectile-project-name) project-mode-command-override-alist))))
      (if project-overides
          (compile (funcall (cdr (assoc major-mode project-overides)) tests))
        (if mode-command
            (let ((default-directory (project-root (project-current t))))
              (compile (funcall mode-command tests))))
        (message "No command found for %s mode" major-mode)))))

(defun buffer-lines-as-list (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (let ((lines '())
          (start (point-min))
          (end (point-max)))
      (goto-char start)
      (while (< (point) end)
        (push (tap--parse-test-line (buffer-substring-no-properties (line-beginning-position) (line-end-position))) lines)
        (forward-line))
      (nreverse lines))))


(defun tap--parse-test-line (input-line)
  "split the line and return cons cell with (file-name . test-name) for test runners which need the test-name"
  (let ((space-pos (string-match " " input-line)))
    (if space-pos
        (let ((car-part (substring input-line 0 space-pos))
              (cdr-part (substring input-line (1+ space-pos))))
          (cons car-part cdr-part))
      nil)))


(defun tap--make-test-string (test-identifier)
  "formats the test-identifier cons cell to a string stored in buffer"
  (concat (car test-identifier) " " (cdr test-identifier)))
