(defun select-current-test-at-point ()
  "Adds the result of `current-test-at-point' to the *test-at-point-selections* buffer."
  (interactive)
  (let ((test-string (current-test-at-point)))
    (with-current-buffer (get-buffer-create "*test-at-point-selections*")
      (goto-char (point-max))
      (insert test-string)
      (insert "\n"))))


(defun unselect-current-test-at-point ()
  "Removes the result of `current-test-at-point' from the *test-at-point-selections* buffer."
  (interactive)
  (let ((test-string (current-test-at-point)))
    (with-current-buffer (get-buffer "*test-at-point-selections*")
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line-start (point)))
          (forward-line 1)
          (let ((line-end (point))
                (line (buffer-substring-no-properties line-start line-end)))
            (when (string= (string-trim line) test-string)
              (delete-region line-start line-end)
              (goto-char (point-min)) ; Restart search from beginning
              )))))))


(defun remove-current-test-at-point-from-buffer ()
  "Removes the result of `current-test-at-point' from the *test-at-point-selections* buffer."
  (interactive)
  (let ((test-string (current-test-at-point)))
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
            (compile (funcall mode-command (buffer-file-name) tests))))
      (message "No command found for %s mode" major-mode))))

(defun buffer-lines-as-list (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (let ((lines nil)
          (start (point-min))
          (end (point-max)))
      (goto-char start)
      (while (< (point) end)
        (push (buffer-substring (line-beginning-position) (line-end-position)) lines)
        (forward-line))
      (nreverse lines))))


(define-key evil-normal-state-map (kbd "SPC c T") 'test-at-point-run-selected)
