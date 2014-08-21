BUILDING and RUNNING
====================

In order to build the test in each of these subdirectories, you must have first
followed the setup instructions in the README for ``concurrent_cilk`` (you can
look in the ``.jenkins_script.sh`` for inspiration).

After you have successfully built concurrent_cilk, you can make the test in any
of the test directories by calling ``make``. The executable will be placed into
a subdirectory (``bin/``) or in the same directory. There is a 1-1
correspondence between ``.c`` and ``.exe`` file names (``%.c -> %.exe``).

In order to run the ``test_cilk.py`` script, you must run it from ``CILK_SRC``.
Note: Directories that are not tested with this script are marked by a file
named ``NOTEST`` -- or, if there is no makefile, we will not try to build in
that directory. Directories (not named ``.git`` or ``build``) that have a
makefile, and no ``NOTEST`` file in them will *automatically* be added to the
tests run by ``test_cilk``.

In order to create new tests, include the various cilk headers that you need
(you can look at some of the files for inspiration on this). In order to setup
the build system for these new files, simply copy ``CMakeLists.txt`` and
``Makefile`` from any directory of your choice (although ``parfib`` is a good
choice).

