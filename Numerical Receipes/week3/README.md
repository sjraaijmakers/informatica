Setting up the environment
==========================

For this Python assignment, and any future Python assignments or applications
you may be working on, it is encouraged to set up a virtual environment, in
order to not pollute your system-wide space of packages. Within the virtual
environment, we'll also set up some prerequisites such as `pep8` and `pyflakes`
to check whether the Python files have been properly formatted and don't contain
redundant code. The virtual environment can be set up as follows:

```
sudo apt-get install python-virtualenv
virtualenv env --python=python3 --system-site-packages
env/bin/pip install pep8 pyflakes
```

Implementation
==============

The framework consists of the following files:

- `integration.py`: this will contain your implementation of the Riemann sum
  with support for the left, middle and right sum; the trapezoidal rule and the
  Simpson rule.
- `monte_carlo.py`: this will contain your implementation of the Monte Carlo
  technique to perform numerical integration. Optionally, it is possible to
  optimise your implementation using `numpy` only. This can be implemented as
  `fast_monte_carlo`.
- `main.py`: this is used to test your implementation with several functions.

It is essential that you properly document and annotate your code using
comments.

Report
======

For this assignment you will have to write a report. The general outline of the
report should be as follows:

- An introduction explaining what your report will be covering.
- A hypothesis which shows what you had been expecting before carrying out the
  experiments.
- A thorough explanation of your implementation on an abstract and mathematical
  level.
- The results of your experiments.
- A comparison of these results as well as a discussion.
- A conclusion.

Make sure to take good care of both the orthography and grammar. Please, do
proof-read your report several times before you hand it in. When comparing the
different results, it may be useful to outline them in a table or visualise them
in a graph.

The report should be stored as `docs/report.pdf`, and it must be written in
LaTeX.

Submission
==========

Using the supplied `Makefile` to pack up the assignment into a tarball is
mandatory, as it will ask you for personal information to include within the
assignment, and as it will check your assignment using `pep8` and `pyflakes`,
before it will pack it all up into a tarball. If all went well, the Makefile
will produce a file `assignment1_{student_ids}.tar.gz` that you can then upload
onto Blackboard.
