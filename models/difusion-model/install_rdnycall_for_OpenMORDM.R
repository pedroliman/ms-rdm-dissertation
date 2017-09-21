url <- "https://cran.r-project.org/src/contrib/Archive/rdyncall/rdyncall_0.7.5.tar.gz"
pkgFile <- "rdyncall_0.7.5.tar.gz"
download.file(url = url, destfile = pkgFile)

# Install dependencies

# install.packages(c("ada", "ipred", "evd"))

# Install package
install.packages(pkgs=pkgFile, type="source", repos=NULL)

# Delete package tarball
unlink(pkgFile)