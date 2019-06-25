.crtlgetNormalizedLines <- function(nbLines, dim) {
  
  if (!(class(nbLines) %in% c("numeric", "integer", "double")) | 
      !(class(dim) %in% c("numeric", "integer", "double"))) {
    stop(paste("nbLines and dim should be numeric, currently :", class(nbLines), 
               class(dim)))
  }
  if (nbLines <= 0) {
    stop(paste("You should ask for at least one line, currently :", nbLines))
  }
  if (dim <= 2) {
    stop(paste("You should ask for at least two-dimensions lines, currently :", 
               dim))
  }
}

.crtldtFormat <- function(dt) {
  
  if (!("data.table" %in%  class(dt))) {
    stop(paste(deparse(substitute(dt)), "should be a data.table, currently :", 
               class(dt)))
  }
  if (nrow(dt) <= 0) {
    stop(paste(deparse(substitute(dt)), "should have at least one line"))
  }
}

.crtlPtdf <- function(dt, dt2 = NULL) {
  
  col_ptdf <- colnames(dt)[grep("ptdf", colnames(dt))]
  if(length(col_ptdf) == 0) {
    stop(paste("You should have ptdf columns to represent the polyhedron dimensions",
               "ex : ptdfFR for France"))
  }
  if (!is.null(dt2)) {
    col_ptdf2 <- colnames(dt2)[grep("ptdf", colnames(dt2))]
    
    if (!all(col_ptdf %in% col_ptdf2) | !all(col_ptdf2 %in% col_ptdf)) {
      stop(paste(deparse(substitute(dt)), "and", deparse(substitute(dt2)),
                 "should have the same ptdf columns, currently :", 
                 paste(col_ptdf, collapse = ", "), "and",
                 paste(col_ptdf2, collapse = ", ")))
    }
  }
  return(col_ptdf)
}

.crtlNumeric <- function(num) {
  
  if (class(num) != "numeric") {
    stop(paste(num, "should be numeric, currently :",class(num)))
  }
  if (length(num) != 1) {
    stop(paste(num, "should be of length one, currently :", length(num)))
  }
}

.crtlBoolean <- function(bool) {
  
  if (class(bool) != "logical") {
    stop(paste(bool, "should be logical, currently :",class(bool)))
  }
  if (length(bool) != 1) {
    stop(paste(bool, "should be of length one, currently :", length(bool)))
  }
}

.crtlgetBestPolyhedron <- function(maxiter, thresholdIndic) {
  
  if (trunc(maxiter) != maxiter) {
    warning(paste("You should put an integer value for maxiter, your value",
                  "has been truncated to", trunc(maxiter)))
    maxiter <- trunc(maxiter)
  }
  if (maxiter < 1) {
    stop(paste("maxiter should be >= 1, currently :", maxiter))
  }
  if (thresholdIndic > 1 | thresholdIndic < 0) {
    stop("The value of thresholdIndic should be between 0 and 1, currently :",
         thresholdIndic)
  }
}