;;; test-at-point.el --- Simply run unit tests in compilation mode based on point position -*- lexical-binding: t; -*-

;; Author: Chris Hipple
;; URL: https://github.com/C-Hipple/test-at-point
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))

;; SPDX-License-Identifier: GPL-3.0+

;;; Commentary:

;; This package provides testing tools which let you find the current test at the cursor position regardless of language and immediately run it in a compilation buffer.
;;
;; Use (run-test-at-point)

;; You can also incrementally add tests (of the same type) to a minibuffer and run all of them with 1 command

;; use (test-at-point-select-test)
;; then (test-at-point-run-selected)

;;; Code:

;; Allow toggling default behavior on always saving all buffers
(setq test-at-point-pre-save t)


(defun go-test-command (file-name test-name)
  (concat "go test -v ./... -run "
          (if (listp test-name)
              (mapconcat 'identity (mapcar (lambda (x) (regexp-quote x)) test-name) "\\|")
            (regexp-quote test-name))))

(defun py-test-command (file-name test-name)
  ;;pytest test_main.py::test_add
  ;;pytest -k test_add
  (concat "pytest -k " test-name))

(defun rust-test-command (file-name test-name)
  (concat "cargo test " test-name))


(setq mode-command-pattern-alist
      '((go-mode . go-test-command)
        (go-ts-mode . go-test-command)
        (python-mode . py-test-command)
        (python-ts-mode . py-test-command)
        (rust-ts-mode . rust-test-command)
        (rust-mode . rust-test-command)
        (rustic-mode . rust-test-command)))


(defun diff-lsp-test-command (file-name test-name)
  (concat "RUST_BACKTRACE=1 cargo test " test-name))

(setq project-mode-command-override-alist
      ;; example with one of my other open source projects
      ;; Setting various rust modes depending on how that emacs is configured.
      '(("diff-lsp" . ((rustic-mode . diff-lsp-test-command)
                       (rust-mode . diff-lsp-test-command)
                       (rust-ts-mode . diff-lsp-test-command)))))

;;;###autoload
(defun run-test-at-point ()
  "Runs the test command based on major mode and test name."
  (interactive)
  (when test-at-point-pre-save
    (save-some-buffers 1))
  (let* ((mode-command (cdr (assoc major-mode mode-command-pattern-alist)))
         (project-overides (cdr (assoc (projectile-project-name) project-mode-command-override-alist))))
    (if project-overides
        (compile (funcall (cdr (assoc major-mode project-overides)) (buffer-file-name) (current-test-at-point)))
      (if mode-command
          (compile (funcall mode-command (buffer-file-name) (current-test-at-point))))
      (message "No command found for %s mode" major-mode))))


(setq mode-test-pattern-alist
      '(
        (go-mode . "^func \\(Test_[a-zA-Z0-9_+]+\\)")
        (go-ts-mode . "^func \\(Test_[a-zA-Z0-9_+]+\\)")
        (python-mode . "^def \\([a-zA-Z0-9_]+\\)")
        (python-ts-mode . "^def \\([a-zA-Z0-9_]+\\)")
        (rust-mode . "fn \\(test_[a-zA-Z0-9_+]+\\)")
        (rust-ts-mode . "fn \\(test_[a-zA-Z0-9_+]+\\)")
        (rustic-mode . "fn \\(test_[a-zA-Z0-9_+]+\\)")))

(defun get-pattern-by-mode ()
  (interactive)
  (let ((mode-pattern (assoc major-mode mode-test-pattern-alist)))
    (if mode-pattern
        (cdr mode-pattern))))


(defun current-test-at-point ()
  (let ((my-line (thing-at-point 'line))
        (pattern (get-pattern-by-mode))
        (result nil))
    (if (string-match pattern my-line)
        (setq result (match-string 1 my-line))
      (save-excursion
        (while (and (re-search-backward pattern nil t 1) (not result))
          (beginning-of-line)
          (let ((this-line (thing-at-point 'line)))
            (when (string-match pattern this-line)
              (setq result (match-string 1 this-line)))))))
    result))


(defun call-current-test-at-point ()
  ;; Debugging helper
  (interactive)
  (let ((res (current-test-at-point)))
    (message (concat "Found above test: " res ))))

(defun call-get-pattern-by-mode()
  ;; Debugging helper
  (interactive)
  (let ((res (get-pattern-by-mode)))
    (message (concat "Found the pattern: " res))
    )
  )

(provide 'test-at-point)

;;; test-at-point.el ends here
