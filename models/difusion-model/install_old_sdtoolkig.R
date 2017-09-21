url <- "https://cran.r-project.org/src/contrib/Archive/sdtoolkit/sdtoolkit_2.31.tar.gz"
pkgFile <- "sdtoolkit_2.31.tar.gz"
download.file(url = url, destfile = pkgFile)

# Install dependencies

# install.packages(c("ada", "ipred", "evd"))

# Install package
install.packages(pkgs=pkgFile, type="source", repos=NULL)

# Delete package tarball
unlink(pkgFile)