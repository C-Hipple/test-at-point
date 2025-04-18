API Reference
============

Core Functions
-------------

``run-test-at-point``
~~~~~~~~~~~~~~~~~~~~

.. code-block:: elisp

   (run-test-at-point)

Main function to run the test at the current cursor position. This function:
- Saves modified buffers if ``test-at-point-pre-save`` is enabled
- Finds the nearest test function above the cursor
- Executes the test in a compilation buffer

Helper Functions
--------------

``current-test-at-point``
~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: elisp

   (current-test-at-point)

Returns the name of the test function at or above the current cursor position.

``get-pattern-by-mode``
~~~~~~~~~~~~~~~~~~~~~

.. code-block:: elisp

   (get-pattern-by-mode)

Returns the test pattern regex for the current major mode.

Debugging Functions
----------------

``call-current-test-at-point``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: elisp

   (call-current-test-at-point)

Interactive function that displays the test name found at the current point.

``call-get-pattern-by-mode``
~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: elisp

   (call-get-pattern-by-mode)

Interactive function that displays the test pattern for the current mode.

Configuration Variables
--------------------

``test-at-point-pre-save``
~~~~~~~~~~~~~~~~~~~~~~~~

Controls whether buffers are automatically saved before running tests.

``mode-test-pattern-alist``
~~~~~~~~~~~~~~~~~~~~~~~~~

Association list mapping major modes to their test pattern regexes.

``mode-command-pattern-alist``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Association list mapping major modes to their test command functions.

``project-mode-command-override-alist``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Association list for project-specific test command overrides.

Built-in Test Command Functions
--------------------

``go-test-command``
~~~~~~~~~~~~~~~~~

.. code-block:: elisp

   (go-test-command test-identifier)

Returns the Go test command string, where test-identifier is a cons cell with the (file-name . test-name)

``py-test-command``
~~~~~~~~~~~~~~~~~

.. code-block:: elisp

   (py-test-command test-identifier)

Returns the Python test command string, where test-identifier is a cons cell with the (file-name . test-name)

``rust-test-command``
~~~~~~~~~~~~~~~~~~

.. code-block:: elisp

   (rust-test-command test-identifier)

Returns the Rust test command string, where test-identifier is a cons cell with the (file-name . test-name) 
