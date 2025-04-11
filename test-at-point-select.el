;;;###autoload
(defun select-current-test-at-point ()
  "Adds the result of `current-test-at-point' to the *test-at-point-selections* buffer."
  (interactive)
  (let ((test-identifier (current-test-at-point)))
    (message (prin1-to-string (type-of test-identifier)))
    (message (concat "file: " (car test-identifier)))
    (message (concat "test: " (cdr test-identifier)))
    (with-current-buffer (get-buffer-create "*test-at-point-selections*")
      (goto-char (point-max))
      (message (concat "Adding test: " (cdr test-identifier)))
      (insert (tap--make-test-string test-identifier))
      (insert "\n"))))


;;;###autoload
(defun remove-current-test-at-point-from-buffer ()
  "Removes the result of `current-test-at-point' from the *test-at-point-selections* buffer."
  (interactive)
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
                (forward-line 0))))))))) ; Ensure forward-line doesn't move unnecessarily

;;;###autoload
(defun test-at-point-run-selected ()
  (interactive)
  (let* ((tests (with-current-buffer "*test-at-point-selections*"
                  (buffer-lines-as-list)))
         (mode-command (cdr (assoc major-mode mode-command-pattern-alist)))
         (project-overides (cdr (assoc (projectile-project-name) project-mode-command-override-alist))))
    (if project-overides
        (compile (funcall (cdr (assoc major-mode project-overides)) (buffer-file-name) tests))
      (if mode-command
          (let ((default-directory (project-root (project-current t))))
            (compile (funcall mode-command tests))))
      (message "No command found for %s mode" major-mode))))

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

(setq debug-on-error 't)
