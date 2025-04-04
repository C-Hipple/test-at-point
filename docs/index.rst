.. toctree::
   :maxdepth: 2
   :caption: Contents:

   installation
   usage
   configuration
   api
   contributing

Welcome to test-at-point
=======================

test-at-point is an Emacs package that provides a simple way to run unit tests based on your cursor position. It supports multiple programming languages and can be easily configured for different projects.

Features
--------

* Run tests at cursor position with a single command
* Support for multiple languages:
  * Go
  * Python
  * Rust
* Project-specific test command overrides
* Automatic buffer saving before test execution
* Compilation mode integration

Quick Start
----------

To get started, install the package and run:

.. code-block:: elisp

   (run-test-at-point)

This will execute the test at your cursor position in a compilation buffer.

For more detailed information, see the :doc:`usage` section.

License
-------

This package is licensed under the GPL-3.0+ license. See the :doc:`LICENSE <../LICENSE>` file for details.
