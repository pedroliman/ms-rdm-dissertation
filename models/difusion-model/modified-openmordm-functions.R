# Modified Versions of the analyze.prim and analyze.cart from the OpenMORDM Package.

#' Determines the vulernabilities due to deep uncertainties using the Patient
#' Rule Induction Method (PRIM).
#' 
#' This method supports both the \code{prim.box} command from the \code{prim}
#' package and the \code{sdprim} command from the \code{sdtoolkit}.  \code{prim.box}
#' is used by default, but can be changed to \code{sdprim} by setting \code{method="sdprim"}.
#' Note that this will require \code{sdtoolkit} to be installed.
#' 
#' We have experienced errors while trying to run sdtoolkit.  We found that using
#' version 2.31 or before works best.  In addition, sdtoolkit tends to crash RStudio.
#' We recommend running this command from a console.
#' 
#' @param factors the sampled deeply uncertain parameterizations
#' @param response vector of responses whose length equals the number of factors
#' @param bounds bounds of the sampled uncertainties
#' @param which.box index of the PRIM box to plot
#' @param show.plot if TRUE, generates a plot representing the PRIM box
#' @param method the method, such as \code{prim} or \code{sdprim}
#' @param ... optional parameters passed to prim.box
#' @export

analyze.prim <- function(factors, response, bounds=NULL, which.box=1, show.plot=TRUE, method="prim", ...) {
  if (method == "prim") {
    message("Chamando algoritmo prim.box")
    box <- prim.box(factors, response, ...)
    message("Iniciando tratamento do OpenMORDM.")
    marks <- lapply(1:box$num.hdr.class, function(i) {
      i <- eval(i)
      colnames(box$box[[i]]) <- colnames(factors)
      mordm.mark.box(box$box[[i]], box$y.fun[i], box$mass[i])
    })
    
    if (is.null(bounds)) {
      bounds <- apply(factors, 2, range)
    }
    
    dummy.data <- list()
    attr(dummy.data, "nvars") <- ncol(factors)
    attr(dummy.data, "bounds") <- bounds
    mordm.plot.box(dummy.data, marks[[which.box]])
    
    # compute density and coverage of the box
    varargs <- list(...)
    
    if (is.null(varargs$threshold.type) || varargs$threshold.type==0) {
      
    } else if (varargs$threshold.type == -1) {
      threshold <- mean(response)
      total.interesting = sum(response <= threshold)
      
      captured.indices <- mordm.select.indices(factors, mordm.mark.union(marks))
      captured.interesting = sum(response[captured.indices] <= threshold)
      
      cat("Coverage: ")
      cat(captured.interesting / total.interesting)
      cat("\n")
      cat("Density: ")
      cat(captured.interesting / length(captured.indices))
      cat("\n")
    } else {
      threshold <- mean(response)
      total.interesting = sum(response >= threshold)
      
      captured.indices <- mordm.select.indices(factors, mordm.mark.union(marks))
      captured.interesting = sum(response[captured.indices] >= threshold)
      
      cat("Coverage: ")
      cat(captured.interesting / total.interesting)
      cat("\n")
      cat("Density: ")
      cat(captured.interesting / length(captured.indices))
      cat("\n")
    }
    
    invisible(marks)
  } else if (method == "sdprim") {
    if (packageVersion("sdtoolkit") > '2.31') {
      warning("Newer version of sdtoolkit have known errors.  Please use version 2.31 or before.")
    } else if (Sys.getenv("RSTUDIO") == "1") {
      warning("sdtoolkit often causes RStudio to crash.  Please run PRIM from the console.")
    }
    
    require(sdtoolkit)
    sdprim(factors, response, ...)
  } else {
    warning("Unknown PRIM method, must be 'prim' or 'sdprim'")
  }
}