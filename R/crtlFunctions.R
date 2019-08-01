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



#' Transform B to antares format
#'
#' @param B \code{data.table}, face for 3 country, BE, DE anf FR
#'
#' @noRd
.fromBtoAntares <- function(face, col_ptdf, areaName){
  B <- face[, .SD, .SDcols = col_ptdf]
  names(B) <- gsub("ptdf", "", names(B))
  nam <- as.character(1:nrow(B))
  nam <- ifelse(nchar(nam)==1, paste0(0, nam), nam)

  
  if (areaName == "cwe_at") {
    coefAntares <- data.table(Name = paste0("FB", nam),
                              BE.FR = round(B$BE - B$FR, 2),
                              DE.FR = round(B$DE - B$FR, 2),
                              DE.NL = round(B$DE, 2),
                              BE.NL = round(B$BE, 2),
                              BE.DE = round(B$BE - B$DE, 2),
                              AT.DE = round(B$AT - B$DE, 2))
  } else if (areaName == "cwe") {
    coefAntares <- data.table(Name = paste0("FB", nam),
                              BE.FR = round(B$BE - B$FR, 2),
                              DE.FR = round(B$DE - B$FR, 2),
                              DE.NL = round(B$DE, 2),
                              BE.NL = round(B$BE, 2),
                              BE.DE = round(B$BE - B$DE, 2))
  } else if (areaName == "other") {
    
  } else {
    stop(paste("The value of areaName must be one of the following :",
               "cwe, cwe_at, other,", "currently :", areaName))
  }


}

.ctrlHubDrop <- function(hubDrop, PTDF) {
  col_ptdf <- colnames(PTDF)[grep("ptdf", colnames(PTDF))]
  if (!grepl("ptdf", names(hubDrop))) {
    ptdf_hubDrop <- c(paste0("ptdf", names(hubDrop)))
  }
  ptdf_hubDrop <- c(ptdf_hubDrop, sapply(names(hubDrop), function(X) {
    sapply(1:length(hubDrop[[X]]), function(i) {
      paste0("ptdf", hubDrop[[X]][i])
    })
  }))
  
  if (!all(ptdf_hubDrop %in% col_ptdf)) {
    warning(paste(paste(
      ptdf_hubDrop[!(ptdf_hubDrop %in% col_ptdf)], collapse = " "),
      "is (are) not in ptdf name"))
  }
  
  if (!all(col_ptdf %in% ptdf_hubDrop)) {
    stop("hubDrop does not contain all the ptdf in PTDF")
  }
}


.crtlCountriesCombn <- function(countries) {
  if(class(countries) == "list") {
    data <- data.frame(rbindlist(lapply(1:length(countries), function(X) {
      if(length(countries[[X]]) != 2) {
        stop(paste("The combination of countries must all be of length 2, currrently for the",
                   X, "element of the list :", length(countries[[X]])))
      }
      data.frame("X1" = countries[[X]][1], "X2" = countries[[X]][2])
    })))
  } else if(class(countries) == "character") {
    data <- data.frame(t(utils::combn(countries, 2)))
  } else {
    stop("countries type can only be list or character")
  }
  setDT(data)
  data <- data[, lapply(.SD, as.character)]
  setDF(data)
  data
}


