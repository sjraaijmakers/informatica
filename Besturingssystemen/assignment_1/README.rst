==================
Assignment 1: 42sh
==================

:Date: March 30th, 2015
:Deadline: April 11th, 2015, 23:59 (anywhere on Earth)

Objectives
==========

You must implement a Unix shell.

Requirements
============

Your shell must not be based on (or exploit) another shell to achieve
the objective. For example, ``system(3)`` is forbidden.  You must
submit your work as a tarball [#]_. Next to the source code, your
archive must contain a text file file named “``AUTHORS``” containing
your name(s) and Student ID(s). You may work in groups of 2.  **It is
strongly advised you use version control.**

.. [#] http://lmgtfy.com/?q=how+to+make+a+tarball

Getting started
===============

1. Unpack the provided source code archive; then run ``make``
2. Try out the generated ``42sh`` and familiarize yourself with its
   interface. Try entering the following commands at its prompt::

        echo hello
        sleep 1; echo world
        ls | more

   To understand the default output, read the file ``ast.h``.

3. Familiarize yourself with using ``valgrind`` to check a program.

4. Read the Wikipedia page on “man pages” [#]_. Read it carefully.

5. Read the manual pages for exit(3), chdir(2), fork(2), execvp(3), wait4(2)
   (and the other ``wait`` functions), pipe(2), dup2(2), sigaction(2).

6. Try and modify the file ``shell.c`` and see the effects for
   yourself. This is probably where you should start implementing your shell.

.. [#] http://en.wikipedia.org/wiki/Man_page

.. note:: Your understanding of the Unix manual and its
          division in sections will be checked during the
          exam. Really pay attention to this.


Grading
=======

Your grade starts from 0, and the following tests determine your grade
(in no particular order):

- +0,5pt if you have submitted an archive in the right format with an ``AUTHORS`` file.
- +0,5pt if your source code builds without errors and you have modified ``shell.c`` in any way.
- +1pt if your shell runs simple commands properly.
- +1pt if your shell recognizes and properly handles the ``exit`` built-in command.
- +1pt if your shell recognizes and properly handles the ``cd`` built-in command.
- +1pt if your shell runs sequences of 3 commands or more properly.
- +1pt if your shell runs pipes of 2 simple commands properly.
- -1pt if ``valgrind`` reports memory errors while running your shell.
- -1pt if ``clang -W -Wall`` reports warnings when compiling your code.

The following extra features will be tested to obtain higher grades,
but only if you have obtained a minimum of 5 points on the list above
already:

- +1pt if your shell runs pipes of more than 2 parts consisting of
  sequences or pipes of simple commands.
- +1pt if your shell supports redirections.
- +0,5pt if your shell supports detached commands.
- +0,5pt if your shell supports executing commands in a sub-shell.
- +1pt if your shell supports the ``set`` and ``unset`` built-ins for
  managing environment variables.
- +2pt if your shell supports simple job control: Ctrl+Z to suspend a
  command group, ``bg`` to continue a job in the background, and
  ``fg`` to recall a job to the foreground.
- -1pt if your shell stops when the user enters Ctrl+C on the terminal, or
  if regular commands are not interrupted when the user enters Ctrl+C.
- -1pt if any of your source files contains functions of more than 30
  lines of code.
- -1pt if your source files are not neatly indented.

The grade will be maximized at 10, so you do not need to implement
all features in this second list to get a top grade.

.. note:: Your shell will be evaluated largely automatically. This
   means features only get a positive grade if they work perfectly, and
   there will be no half grade for "effort".

.. raw:: pdf

   PageBreak

Example commands
================

.. code:: sh

   ## simple commands:
   ls
   sleep 5   # must not show the prompt too early

.. code:: sh

   ## simple commands, with built-ins:
   mkdir t
   cd t
   /bin/pwd  # must show the new path
   exit 42   # terminate with code

.. code:: sh

   ## sequences:
   echo hello; echo world # must print in this order
   exit 0; echo fail  # must not print "fail"

.. code:: sh

   ## pipes:
   ls | grep t
   ls | more    # must not show prompt too early
   ls | sleep 5 # must not print anything, then wait
   sleep 5 | ls # must show listing then wait
   ls /usr/lib | grep net | cut -d. -f1 | sort -u

.. code:: sh

   ## redirects:
   >dl1 ls /bin; <dl1 wc -l
   >dl2 ls /usr/bin; >>dl1 cat dl2 # append
   <dl2 wc -l; <dl1 wc -l # show the sum
   >dl3 2>&1 find /var/. # errors redirected
   2>&1 >/dev/null find /var/. | more # only errors

.. code:: sh

   ## detached commands:
   sleep 5 &  # print prompt early
   { sleep 1; echo hello }& echo world; sleep 3 # invert output

.. code:: sh

   ## sub-shell:
   ( exit 0 ) # top shell does *not* terminate
   cd /tmp; /bin/pwd; ( cd /bin ); /bin/pwd # "/tmp" twice

.. code:: sh

   ## environment variables
   set hello=world; env | grep hello # prints "hello=world"
   (set top=down); env | grep top # does not print "top=down"
   unset PATH; cat   # prints an error: "cat" not found
