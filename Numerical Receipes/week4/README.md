# Assignment Numerical Integrals
Prerequisites:
~~~
sudo apt-get install pep8 pyflakes make
~~~
~~~
git clone git@github.com:icyrizard/Numerical-Recipes-UvA.git
~~~

Don't forget to:
+ Install pep8 and pyflakes, **VERY IMPORTANT!**
+ Add your report in the docs folder of the assigment, and call it report.pdf.
+ Put your names in the $(STUDENT_NAMES) variable in the makefile.
+ Put your names in all the python files
+ Run `make` frequently to prevent a lot of work later on.

## Framework guide
Use this framework to do the assignment of Numerical Integrals. This is mainly
to prevent students from writing spaghetti python code, and to speed-up the
task of grading.

Fill in the code in the apporiate places. All the files correspond with the
assignments on uva.sowiso.nl. You may alter the functions if needed, but let
the general idea of the framework the same.

+ Makefile
    The makefile included in this directory is essential for this handing in
    your code. Running `make` will run pep8 and pyflakes when some change has
    been made to your python code. It creates a deliverable every time you run
    make. But before it does, the code is checked for syntax using pep8 and
    unused or undefined variables.

## Hand in your code
Hand in the tar.gz that is created when you run make. Upload this file in
blackboard, **JUST ONES PER STUDENT**. Make only creates the archive if you
removed all of your bugs and style issues.

## Report
In ass1/docs/ you should put your report called: `report.pdf`. This will be
included in the archive when you run make. You can do whatever you like in the
docs folder, just name your report of the assignment `report.pdf`.


## Problems
+ If you can not install pep8 or pyflakes or make, you should be ashamed.
+ For any other problems you can contact any of the student assistents.
    + Richard Torenvliet - rich.riz@gmail.com
