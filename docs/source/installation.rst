Installation
============

Requirements
-----------

* Emacs 25.1 or later
* Projectile (recommended for project-specific configurations)

Package Manager Installation
--------------------------

Using straight.el:

.. code-block:: elisp

   (straight-use-package
    '(test-at-point :type git :host github :repo "C-Hipple/test-at-point"))

Using package.el:

.. code-block:: elisp

   (package-install 'test-at-point)

Manual Installation
-----------------

1. Clone the repository:

.. code-block:: bash

   git clone https://github.com/C-Hipple/test-at-point.git

2. Add the following to your Emacs configuration:

.. code-block:: elisp

   (add-to-list 'load-path "/path/to/test-at-point")
   (require 'test-at-point)

Configuration
------------

Add the following to your Emacs configuration to enable automatic buffer saving before test execution:

.. code-block:: elisp

   (setq test-at-point-pre-save t)

For project-specific test command overrides, see the :doc:`configuration` section. 