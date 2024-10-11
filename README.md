# test-at-point
Emacs Package for easily sending unit tests to compile-mode


This package adds some simple functions for allowing you to quickly run individual unit tests across a variety of major-modes using compile mode.


## Usage

Calling `run-test-at-point` will lookup the test at the current point via regular expression searching and then run that only that test via `compile-mode`.

Add your preferred keybinding to your init.

```elisp
(define-key evil-normal-state-map (kbd "SPC c t") 'run-test-at-point)
```

## Installation

Use your favorite package manager to install from this github repository

### Spacemacs

(I use spacemacs, but any emacs should work)

```elisp
dotspacemacs-additional-packages '(
  ...
  (code-review :location (recipe
    :fetcher github
    :repo "C-Hipple/test-at-point"
    :files ("*.el")))
  ...
)
```


## Extending functionality

Currently only python, go, and rust are supported.  To add more languages add values do the following:
1. Add the regex which finds a test function name to the alist `mode-test-pattern-alist`
2. Add a command builder function and link it to the major mode via `mode-command-pattern-alist`


## Setting custom test command functions

A lot of larger projects will have custom tooling or specific commands which need to be used in order to run tests.  

In the below example we override the rust test command builder to always use the environment variable as RUST_BACKTRACE=1.


```elisp

(defun diff-lsp-test-command (file-name test-name)
  (concat "RUST_BACKTRACE=1 cargo test " test-name))

(setq project-mode-command-overide-alist
      ;; example with one of my other open source projects
      ;; Setting various rust modes depending on how that emacs is configured.
      '(("diff-lsp" . ((rustic-mode . diff-lsp-test-command),
                       (rust-mode . diff-lsp-test-command),
                       (rust-ts-mode . diff-lsp-test-command)))))
```


