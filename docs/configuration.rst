Configuration
=============

Global Configuration
------------------

The following variables can be set in your Emacs configuration:

``test-at-point-pre-save``
~~~~~~~~~~~~~~~~~~~~~~~~~

Controls whether all buffers are automatically saved before running tests.

.. code-block:: elisp

   (setq test-at-point-pre-save t)  ; Enable automatic saving (default)
   (setq test-at-point-pre-save nil) ; Disable automatic saving

Test Command Patterns
-------------------

The package uses the following variables to define test patterns and commands:

``mode-test-pattern-alist``
~~~~~~~~~~~~~~~~~~~~~~~~~~

Defines the regex patterns used to identify test functions in different modes.

.. code-block:: elisp

   (setq mode-test-pattern-alist
         '((go-mode . "^func \\(Test_[a-zA-Z0-9_+]+\\)")
           (python-mode . "^def \\([a-zA-Z0-9_]+\\)")
           (rust-mode . "fn \\(test_[a-zA-Z0-9_+]+\\)")))

``mode-command-pattern-alist``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Defines the test commands for different modes.

.. code-block:: elisp

   (setq mode-command-pattern-alist
         '((go-mode . go-test-command)
           (python-mode . py-test-command)
           (rust-mode . rust-test-command)))

Project-Specific Configuration
---------------------------

You can override the default test commands for specific projects using:

``project-mode-command-override-alist``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: elisp

   (setq project-mode-command-override-alist
         '(("project-name" . ((rust-mode . custom-test-command)
                             (python-mode . custom-test-command)))))

Custom Test Commands
------------------

You can define custom test commands for specific modes:

.. code-block:: elisp

   (defun custom-test-command (file-name test-name)
     "Custom test command for specific project."
     (concat "custom-test-command " test-name))

Adding Support for New Languages
------------------------------

To add support for a new language, you need to:

1. Define a test pattern in ``mode-test-pattern-alist``
2. Define a test command function.  The interface for the test function is that it should take a cons cell of (file-name . test-name).  It should return a string of the full test command as a string.
3. Add the mode and command to ``mode-command-pattern-alist``

Example for a new language:

.. code-block:: elisp

   (defun new-lang-test-command (file-name test-name)
     (concat "new-lang-test " test-name))

   (add-to-list 'mode-test-pattern-alist
                '(new-lang-mode . "^test \\([a-zA-Z0-9_]+\\)"))

   (add-to-list 'mode-command-pattern-alist
                '(new-lang-mode . new-lang-test-command)) 
