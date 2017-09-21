OpenMORDM Supplemental Materials
--------------------------------

These materials accompany the in-review research article:

    Hadka, D., J. Herman, P. Reed, and K. Keller.
    An Open Source Framework for Many-Objective Robust Decision
    Making.  Accepted to Environmental Modelling & Software.
    DOI: 10.1016/j.envsoft.2015.07.014

These codes should run on Unix, Linux, Windows, and Mac computers.


---------------------
Required Dependencies
---------------------

Prior to installing OpenMORDM, you must install the following dependencies.
Refer to the appropriate section for your operating system.

Windows:
--------
    1) R version 3.0.2 or later

    2) Install R Tools (http://cran.r-project.org/bin/windows/Rtools/)

    3) Add R Tools to your PATH environment variable
         a) Typically, the path is C:\Program Files\Rtools\bin

    4) Install MinGW and MSYS

    5) Add MinGW and MSYS to your PATH environment variable
         a) Typically, the paths are:
               C:\MinGW\bin
               C:\MinGW\msys\1.0\bin

Linux (tailored for Ubuntu 12.04 LTS):
--------------------------------------
    1) R version 3.0.2 or later (e.g., sudo apt-get install r-base)

          * The default version of R may be older than version 3.0.2.
            If this happens, see http://askubuntu.com/a/614715 for
            directions on installing the latest version.

    2) Install GCC and Make (e.g., sudo apt-get install build-essential)

    3) Install OpenGL: (e.g., sudo apt-get install libsdl1.2debian libsdl1.2-dev libgl1-mesa-dev libglu1-mesa-dev libsdl-image1.2 libsdl-image1.2-dev)

    4) Install Curl:

          * sudo apt-get build-dep libcurl4-gnutls-dev
          * sudo apt-get install libcurl4-gnutls-dev
          * sudo apt-get install libxml2-dev

Mac OS:
-------
    1) R version 3.0.2 or later

    2) Install XCode

We also recommend installing R Studio as it provides a nice GUI for editing
and running R code.


-------------------------
Installation of OpenMORDM
-------------------------

    1) Start R

    2) Set R's working directory to the "OpenMORDM Supplemental Materials" folder:
          * setwd("/path/to/OpenMORDM Supplemental Material")

    3) If using Windows, run:
          * options(devtools.install.args="--force-biarch")

    4) Run the following commands:
          * install.packages("devtools")
          * library(devtools)
          * install_local("rdyncall")
          * install_local("shinyRGL")
          * install_local("OpenMORDM")
          * install_local("rgl")

Note: If using Linux, you should start R with root privileges (e.g., sudo R)


-----------------------------
Generating Figures from Paper
-----------------------------

Run the commands in precomputedDemo.R to recreate the figures appearing in the
manuscript.  These commands load the same dataset used in the original study.


-------------------
Recreating Analysis
-------------------

To recreate the entire analysis appearing in the manuscript, including
generating your own dataset, follow the instructions below.  Since the
entire dataset will be recreated, this process will take several hours.
Also, results may differ slightly from those seen in the paper due to
random effects.

    1) Compile the Borg MOEA
         a) Navigate to the Borg-1.8 folder
         b) Run 'make'
         c) Copy borg.exe into the "OpenMORDM Supplemental Materials" folder

    2) Compile the Lake Problem
         a) Navigate to the LakeProblem folder
         b) Run 'make'
         c) Copy lake.exe into the "OpenMORDM Supplemental Materials" folder

    3) Run the commands in lakeProblemDemo.R


-----------------
Web Visualization
-----------------

OpenMORDM also includes a web-based visualization toolkit.  The web server is
launched from R and provides various 3D and 2D plots and analyses accessible
through a web browser.  For example, using the built-in iris dataset, one can
create the web visualizations with the command:

    library(OpenMORDM)
    explore(iris)

See the documentation for the explore command for additional instructions.


-------------
Documentation
-------------

All OpenMORDM commands are documented and accessible within R by prefixing
a question mark to the command.  For example:

    library(OpenMORDM)
    ?OpenMORDM
    ?mordm.plot

Furthermore, all of this documentation is also available in the OpenMORDM.pdf
document.


-------------------------------
Upgrading from Previous Version
-------------------------------

Please see the file CHANGES.txt for a list of changes to this software and
upgrade instructions.


---------------
Troubleshooting
---------------

* OpenMORDM supports two versions of PRIM.  The interactive sdtoolkit version,
  enabled by the method="sdprim" argument to analyze.prim, can be error-prone.
  If errors are encountered, please use the default PRIM implementation.

* Any time you start a new R session, be sure to set the working directory:
    setwd("/path/to/OpenMORDM Supplemental Material")
  Otherwise, you may see "unable to find executable" or "missing file" errors.

* Cygwin users will need to prepend "./" to the executable names in R.  For
  example, "./lake.exe".  We recommend using MinGW instead and do not extensively
  test with Cygwin.

* If you see an error indicating a package is not available for your R version,
  such as "package devtools not available for 3.0.1", please upgrade to a newer
  version of R.

* Mac OS X users who see an error about llvm-g++-4.2 missing, run:
    * cd /usr/bin
    * sudo ln -fs clang llvm-gcc-4.2
    * sudo ln -fs clang++ llvm-g++-4.2

* Some rgl commands may output the warning "internal error -3 in R_decompress1".
  It does not appear this warning affects the software in any way, and is likely
  safe to ignore.
