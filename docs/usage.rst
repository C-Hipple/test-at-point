Usage
=====

Basic Usage
----------

To run a test at the current cursor position, use:

.. code-block:: elisp

   (run-test-at-point)

This command will:
1. Save any modified buffers (if ``test-at-point-pre-save`` is enabled)
2. Find the nearest test function above the cursor
3. Execute the test in a compilation buffer

Supported Languages
-----------------

The package supports the following languages and their corresponding test patterns:

Go
~~

* Test pattern: ``^func \(Test_[a-zA-Z0-9_+]+\)``
* Command: ``go test -v -run <test-name>``

Python
~~~~~~

* Test pattern: ``^def \([a-zA-Z0-9_]+\)``
* Command: ``pytest -k <test-name>``

Rust
~~~~

* Test pattern: ``fn \(test_[a-zA-Z0-9_+]+\)``
* Command: ``cargo test <test-name>``

Project-Specific Overrides
------------------------

You can override the default test commands for specific projects using ``project-mode-command-override-alist``:

.. code-block:: elisp

   (setq project-mode-command-override-alist
         '(("project-name" . ((rust-mode . custom-test-command)
                             (python-mode . custom-test-command)))))

Debugging
--------

The package provides two helper functions for debugging:

.. code-block:: elisp

   (call-current-test-at-point)  ; Shows the test name found at point
   (call-get-pattern-by-mode)    ; Shows the test pattern for current mode

These can be helpful when troubleshooting test detection issues. 