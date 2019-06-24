.crtlgetNormalizedLines <- function(dtLines, dim) {
  
  if (class(nbLines) != "numeric" | class(dim) != "numeric") {
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