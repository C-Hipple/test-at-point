;;; test-at-point.el --- Simply run unit tests in compilation mode based on point position -*- lexical-binding: t; -*-

;; Author: Chris Hipple
;; URL: https://github.com/C-Hipple/test-at-point
;; Version: 1.0.2
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


(defun go-test-command (test-identifier)
  "test-identifer is a cons cell of ('file-name.go' . 'test-name') or it's a list of concells"
  (message "starting")
  (concat "go test -v ./... -run "
          (if (tap-is-single-cons-cell test-identifier)
              (cdr test-identifier)
            (mapconcat 'identity (map 'list (lambda (x) (cdr x)) test-identifier) "\\|"))))

(defun py-test-command (test-identifier)
  ;;pytest test_main.py::test_add
  ;;pytest -k test_add
  (concat "pytest -k " (cdr test-identifier)))

(defun rust-test-command (test-identifier)
  (concat "cargo test " (cdr test-identifier)))

(defun ts-test-command (test-identifier)
  "TODO: allow setting yarn vs npm vs npx?"
  (format "yarn run test -t \'%s\' --testPathPattern %s"  (cdr test-identifier) (file-name-nondirectory buffer-file-name)))


(setq mode-command-pattern-alist
      '((go-mode . go-test-command)
        (go-ts-mode . go-test-command)
        (python-mode . py-test-command)
        (python-ts-mode . py-test-command)
        (rust-ts-mode . rust-test-command)
        (rust-mode . rust-test-command)
        (typescript-mode . ts-test-command)
        (typescript-tsx-mode . ts-test-command)
        (typescript-tsx-ts-mode . ts-test-command)
        (rustic-mode . rust-test-command)))


(defun diff-lsp-test-command (test-identifier)
  (concat "cargo test " (cdr test-identifier)))

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
         (project-overides (cdr (assoc (projectile-project-name) project-mode-command-override-alist)))
         (project-mode-command (and project-overides (cdr (assoc major-mode project-overides)))))
    (if project-mode-command
        (compile (funcall project-mode-command (current-test-at-point)))
      (if mode-command
          (compile (funcall mode-command (current-test-at-point)))
        (message "No command found for %s mode" major-mode)))))


(setq mode-test-pattern-alist
      '(
        (go-mode . "^func \\(Test\\(_?\\)[a-zA-Z0-9_+]+\\)")
        (go-ts-mode . "^func \\(Test\\(_?\\)[a-zA-Z0-9_+]+\\)")
        (python-mode . "^def \\([a-zA-Z0-9_]+\\)")
        (python-ts-mode . "^def \\([a-zA-Z0-9_]+\\)")
        (rust-mode . "fn \\(test_[a-zA-Z0-9_+]+\\)")
        (rust-ts-mode . "fn \\(test_[a-zA-Z0-9_+]+\\)")
        (rustic-mode . "fn \\(test_[a-zA-Z0-9_+]+\\)")
        (typescript-mode . "\\(?:it\\|test\\)\\s-*(\\s-*['\"]\\([^'\"]+\\)['\"]")
        (typescript-tsx-mode . "\\(?:it\\|test\\)\\s-*(\\s-*['\"]\\([^'\"]+\\)['\"]")
        (typescript-tsx-ts-mode . "\\(?:it\\|test\\)\\s-*(\\s-*['\"]\\([^'\"]+\\)['\"]")))

(defun get-pattern-by-mode ()
  (interactive)
  (let ((mode-pattern (assoc major-mode mode-test-pattern-alist)))
    (if mode-pattern
        (cdr mode-pattern))))


(defun current-test-at-point ()
  "returns a cons cell of (file-name . test-name) using pattern by mode"
  (let ((my-line (thing-at-point 'line))
        (pattern (get-pattern-by-mode))
        (found-test nil))
    (if (string-match pattern my-line)
        (setq found-test (match-string 1 my-line))
      (save-excursion
        (while (and (re-search-backward pattern nil t 1) (not found-test))
          (beginning-of-line)
          (let (
                (this-line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
            (when (string-match pattern this-line)
              (setq found-test (match-string 1 this-line)))))))
    (create-test-cons-cell found-test)))


(defun create-test-cons-cell (test-)
  "takes the test string that was found and builds the cons cell using the relative project path."
  (cons (replace-regexp-in-string (projectile-project-root) "" (buffer-file-name)) test-))


(defun call-current-test-at-point ()
  ;; Debugging helper
  (interactive)
  (let ((res (current-test-at-point)))
    (message (concat "Found above test: " (cdr res)))
    (message (concat "in the file: " (car res)))
    ))

(defun call-get-pattern-by-mode()
  ;; Debugging helper
  (interactive)
  (let ((res (get-pattern-by-mode)))
    (message (concat "Found the pattern: " res))
    )
  )

(defun tap-is-single-cons-cell (variable)
  "Check if VARIABLE is a single cons cell (cdr is not a cons)."
  (and (consp variable)
       (not (consp (car variable)))))

(provide 'test-at-point)

;;; test-at-point.el ends here
