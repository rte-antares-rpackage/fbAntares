.crtlgetNormalizedLines <- function(nbLines, dim) {
  ## Checking if the input arguments of getNormalizedLines are well chosen
  if (!(class(nbLines) %in% c("numeric", "integer", "double")) | 
      !(class(dim) %in% c("numeric", "integer", "double"))) {
    stop(paste("nbLines and dim should be numeric, currently :", class(nbLines), 
               class(dim)))
  }
  if (nbLines <= 0) {
    stop(paste("You should ask for at least one line, currently :", nbLines))
  }
  if (dim < 2) {
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
  ## Checking if the ptdf are well entered
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
  ## Checking if the arguments for getBestPolyhedron are clear
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
.fromBtoAntares <- function(face, col_ptdf, areaConf){
  ## using of the areNames to write the constraints into Antares with the weight file
  
  
  B <- face[, .SD, .SDcols = col_ptdf]
  names(B) <- gsub("ptdf", "", names(B))
  nam <- as.character(1:nrow(B))
  maxnchar <- max(nchar(nam))
  nam <- ifelse(nchar(nam)==1, paste0(0, nam), nam)
  if(maxnchar == 3) {
    nam <- ifelse(nchar(nam)==2, paste0(0, nam), nam)
  }
  
  
  R <- lapply(areaConf$antares[[1]], function(X){
    D <-  round(B[,eval(parse(text = X[2]))], 2)
    D <- data.table(D)
    setnames(D, "D", X[1])
    D
  })

  coefAntares <- Reduce(cbind,   c(data.table(Name = paste0("FB", nam)), R))
  setnames(coefAntares, 'init', 'Name')
  coefAntares
  
}


.fromBtoAntaresvirtualFBarea <- function(face, col_ptdf) {
  B <- face[, .SD, .SDcols = col_ptdf]
  
  colnames(B) <- paste0(gsub("ptdf", "", colnames(B)), ".ZZ_flowbased")
  nam <- as.character(1:nrow(B))
  maxnchar <- max(nchar(nam))
  nam <- ifelse(nchar(nam)==1, paste0(0, nam), nam)
  if(maxnchar == 3) {
    nam <- ifelse(nchar(nam)==2, paste0(0, nam), nam)
  }
  B[, Name := paste0("FB", nam)]
  setcolorder(B, "Name")
  B
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

.crtlFixFaces <- function(fixFaces, col_ptdf) {
  col <- colnames(fixFaces)
  if(!all(col == c("func", "zone"))) {
    stop(paste("The colnames of fixFaces must be func and zone in this order.",
               "Currently :", paste(col, collapse = ", ")))
  }
  valfunc <- unique(fixFaces$func)
  valzone <- unique(fixFaces$zone)
  if(!all(valfunc %in% c("min", "max"))) {
    stop(paste("The values of func in fixFaces must be min or max, currently :", 
               paste(valfunc, collapse = ", ")))
  }
  if(!all(valzone %in% gsub("ptdf", "", col_ptdf))) {
    stop(paste("The values of zone in fixFaces must be in the ptdf colnames, which are :", 
               paste(gsub("ptdf", "", col_ptdf), collapse = ", "), "currently :", 
               paste(valzone, collapse = ", ")))
  }
}

.getFixRams <- function(fixFaces, VERTRawDetails) {
  zone <- NULL
  dtFixRam <- rbindlist(lapply(1:nrow(fixFaces), function(X) {
    func <- fixFaces[X, func]
    if (func == "min") {
      ramVal <- VERTRawDetails[get(fixFaces[X, zone]) == min(get(fixFaces[X, zone])),
                               get(fixFaces[X, zone])]
    } else if (func == "max") {
      ramVal <- VERTRawDetails[get(fixFaces[X, zone]) == max(get(fixFaces[X, zone])),
                               get(fixFaces[X, zone])]
    } 
    data.table(zone = fixFaces[X, zone], ram = ramVal)
  }))
}

.checkDayInTS <- function(ts, calendar) {
  namesWe <- unique(calendar$class[grepl("We", calendar$class)])
  namesWd <- unique(calendar$class[grepl("Wd", calendar$class)])
  
  namesWe <- c("summerWe", "winterWe", "interSeasonWe")
  namesWd <- c("summerWd", "winterWd", "interSeasonWd")
  
  uniqsummerWe <- unique(unlist(unname(unique(
    ts[Date %in% calendar[class == "summerWe"]$time, .SD, .SDcols = !"Date"]))))
  uniqsummerWd <- unique(unlist(unname(unique(
    ts[Date %in% calendar[class == "summerWd"]$time, .SD, .SDcols = !"Date"]))))
  
  uniqwinterWe <- unique(unlist(unname(unique(
    ts[Date %in% calendar[class == "winterWe"]$time, .SD, .SDcols = !"Date"]))))
  uniqwinterWd <- unique(unlist(unname(unique(
    ts[Date %in% calendar[class == "winterWd"]$time, .SD, .SDcols = !"Date"]))))
  
  uniqinterSeasonWe <- unique(unlist(unname(unique(
    ts[Date %in% calendar[class == "interSeasonWe"]$time, .SD, .SDcols = !"Date"]))))
  uniqinterSeasonWd <- unique(unlist(unname(unique(
    ts[Date %in% calendar[class == "interSeasonWd"]$time, .SD, .SDcols = !"Date"]))))
  
  if(!all(uniqsummerWe %in% "4")) {
    stop(paste("The summer weekend days can only have 4 as typical day value,",
               "however some of them have the value(s) :", 
               paste(uniqsummerWe, collapse = ", ")))
  }
  if(!all(uniqsummerWd %in% c("1", "2", "3"))) {
    stop(paste("The summer working days can only have 1, 2 or 3 as typical day value,",
               "however some of them have the value(s) :", 
               paste(uniqsummerWd, collapse = ", ")))
  }
  if(!all(uniqwinterWe %in% "8")) {
    stop(paste("The summer weekend days can only have 8 as typical day value,",
               "however some of them have the value(s) :", 
               paste(uniqwinterWe, collapse = ", ")))
  }
  if(!all(uniqwinterWd %in% c("5", "6", "7"))) {
    stop(paste("The summer weekend days can only have 5, 6 or 7 as typical day value,",
               "however some of them have the value(s) :", 
               paste(uniqwinterWd, collapse = ", ")))
  }
  if(!all(uniqinterSeasonWe %in% "12")) {
    stop(paste("The summer weekend days can only have 12 as typical day value,",
               "however some of them have the value(s) :", 
               paste(uniqinterSeasonWe, collapse = ", ")))
  }
  if(!all(uniqinterSeasonWd %in% c("9", "10", "11"))) {
    stop(paste("The summer weekend days can only have 9, 10 or 11 as typical day value,",
               "however some of them have the value(s) :", 
               paste(uniqinterSeasonWd, collapse = ", ")))
  }
  
}
