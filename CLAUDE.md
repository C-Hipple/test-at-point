# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

test-at-point is an Emacs package that runs individual unit tests from the cursor position across multiple languages (Go, Python, Rust, TypeScript/JavaScript) using Emacs' compile mode.

## Build & Development

- **No build step required** — pure Emacs Lisp, loaded directly
- **No formal test suite** — use debug functions `call-current-test-at-point` and `call-get-pattern-by-mode` for manual verification
- **Documentation:** `cd docs && make html` (Sphinx, deployed to ReadTheDocs)

## Architecture

Two files comprise the entire package:

- **test-at-point.el** — Core package. Mode-based dispatch: detects test name via regex (`mode-test-pattern-alist`), builds a shell command via language-specific command builder (`mode-command-pattern-alist`), and runs it through `compile()`. Entry point is `run-test-at-point`.
- **test-at-point-select.el** — Multi-test selection. Accumulates tests in `*test-at-point-selections*` buffer. **Only supports Go and Python modes** (controlled by `mode-supports-multi-select-alist`). Interactive commands:
  - `select-current-test-at-point` — Add current test to selection
  - `remove-current-test-at-point-from-buffer` — Remove current test from selection
  - `test-at-point-show-selected` — Display selected tests buffer
  - `test-at-point-clear-selected` — Clear all selected tests
  - `test-at-point-run-selected` — Run all selected tests

## Key Data Flow

1. `run-test-at-point` → looks up regex for current major mode → searches backward from point
2. Match produces a cons cell `(relative-file-path . test-name)`
3. Cons cell passed to language-specific command builder (e.g., `go-test-command`) → returns shell command string
4. Command string passed to `compile()`

## Adding Language Support

### Single test execution:
1. Add regex to `mode-test-pattern-alist` (maps major mode → test detection pattern)
2. Write a command builder function taking a cons cell `(file . test-name)`, returning a command string
3. Register it in `mode-command-pattern-alist`

### Multi-test selection support:
To enable `select-current-test-at-point` for a language, the command builder must handle **both** a single cons cell and a list of cons cells. Currently Go and Python support this:
1. Update the command builder to check if input is a list and handle joining multiple test names (see `go-test-command` and `py-test-command` for examples)
2. Add the mode to `mode-supports-multi-select-alist` in `test-at-point-select.el`

**Examples:**
- Go joins tests with `\|`: `go test -v ./... -run TestA\|TestB`
- Python uses file::test syntax: `pytest file1.py::test_a file2.py::test_b`

## Supported Modes

Each language has both standard and tree-sitter mode variants:
- **Go:** `go-mode`, `go-ts-mode` → `go test -v ./... -run`
- **Python:** `python-mode`, `python-ts-mode` → `pytest -k`
- **Rust:** `rust-mode`, `rust-ts-mode`, `rustic-mode` → `cargo test`
- **TypeScript/JS:** `typescript-mode`, `typescript-ts-mode`, `typescript-tsx-mode`, `typescript-tsx-ts-mode` → `yarn run test`

## Configuration

- `project-mode-command-override-alist` allows per-project command overrides keyed by project name
- `test-at-point-pre-save` (default `t`) auto-saves buffers before test runs
