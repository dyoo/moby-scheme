Differences from the main branch:

    * Javascript backend.

    * Support for Intermediate Student Language.


Quick start

  $ git clone git://github.com/dyoo/moby-scheme.git moby
  $ cd moby
  $ git checkout origin/devel

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
  kernel.js           world.html

Open up world.html in your favorite web browser, and you should see a
cow falling from the sky.
