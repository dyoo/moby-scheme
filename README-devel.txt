Differences from the main branch:

    * Javascript backend.

    * Support for Intermediate Student Language.


----------------------------------------------------------------------

Quick start

  $ git clone git://github.com/dyoo/moby-scheme.git moby
  $ cd moby
  $ git checkout origin/devel
  $ git submodule init
  $ git submodule update

Once the devel branch is checked out, you can compile one of the test
programs to see that things are running:

  $ cd src/test
  $ mred ../moby.ss -t js falling-cow.ss

The '-t js' tells the compiler to use the Javascript backend.  The
resulting compiled code is written to a directory camel-case-named by
the source.

  $ cd FallingCow
  $ ls
  image-0.png         main.js                 world.js
  jsunittest          test-kernel.html
  kernel.js           index.html

Open up index.html in your favorite web browser, and you should see a
cow falling from the sky.


----------------------------------------------------------------------

File summary

src/beginner-to-javascript.ss: the backend compiler for Javascript.
Note that the file is written in a special language "lang.ss"; this is
intentional, as we want to be able to bootstrap the compiler.



src/bootstrap-js.compiler.ss: the bootstrapper.  Writes out the result
of feeding beginner-to-javascript.ss to itself into the file
support/js/compiler.js.
