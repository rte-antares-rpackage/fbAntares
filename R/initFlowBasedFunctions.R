#' Load weight file
#'
#' @param weight \code{character} path
#' @param areaConf \code{data.table} area conf
#'
#' @return weight \code{data.table} weight file
#'
#' @noRd
.getWeight <- function(weight, areaConf){
  weight <- data.table::fread(weight, sep = "\t", dec = ".")
  names(weight) <- gsub(" ", "", names(weight))
  names(weight) <- names(weight)%>>%
    tolower()
  
  ##New version
  nameCheck <- c("name", areaConf$links[[1]])
  any(grepl("zz_flowbased", tolower(names(weight))))
  #### Modif new version virtual area
  if (!any(grepl("zz_flowbased", tolower(names(weight))))) {
    if(any(!names(weight) %in% nameCheck)){
      stop(paste0("Names of weight.txt must be name ", paste(nameCheck, collapse = " ")))
    }
  }
  recontructName <- 1:nrow(weight)
  recontructName <- as.character(recontructName)
  maxnchar <- max(nchar(recontructName))
  recontructName <- ifelse(nchar(recontructName)==1, paste0(0, recontructName), recontructName)
  if(maxnchar == 3) {
    recontructName <- ifelse(nchar(recontructName)==2, paste0(0, recontructName), recontructName)
  }
  # recontructName <- ifelse(nchar(recontructName) == 1, paste0(0, recontructName), 
  #                          recontructName)
  recontructName <- paste0("FB", recontructName)
  
  if(any(recontructName != weight$name)){
    stop(paste0("name column of weight.txt must contain in order : ", 
                paste0(recontructName, collapse = ","),
                " ||  actualy : ", paste0( weight$name, collapse = ",")))
  }
  
  names(weight) <- gsub(x=names(weight), pattern =  "[.]", replacement = "%")
  weight
}


#' Load second member file
#'
#' @param secondMember \code{character} path
#'
#' @return secondMember \code{data.table} secondMember file
#'
#' @noRd
.getSecondMember <- function(secondMember){
  secondMember <- data.table::fread(secondMember, sep = "\t", dec = ".")
  if(!all( c("Id_day", "Id_hour", "vect_b", "Name") %in% names(secondMember))){
    stop("Names of second_member.txt must contains Id_day, Id_hour, vect_b, Name")
  }
  secondMember$vect_b <- round(secondMember$vect_b, 0)
  nameConstraints <- sort(unique(secondMember$Name))
  nam <- 1:length(nameConstraints)
  maxnchar <- max(nchar(nam))
  nam <- ifelse(nchar(nam)==1, paste0(0, nam), nam)
  if(maxnchar == 3) {
    nam <- ifelse(nchar(nam)==2, paste0(0, nam), nam)
  }
  nam <- paste0("FB", nam)
  if(!(all(nameConstraints == nam))){
    stop("Problem in name of constraints in file second_member.txt")
  }
  
  secondMember[,.SD, .SDcols = c("Id_day", "Id_hour", "vect_b", "Name")]
}


#' Load daytype file
#'
#' @param daytype \code{character} path
#'
#' @return daytype \code{data.table} daytype file
#'
#' @noRd
.getDayType <- function(daytype){
  daytype <- data.table::fread(daytype, sep = "\t", dec = ".", header = TRUE)
  
  if(dim(daytype)[2] == 1){
    stop("Problem in ts.txt format, sep must be tabulation")
  }
  #Control format date
  
  if(!"Date" %in% names(daytype)){
    stop('Missing Date column in ts.txt (must have a D uppercase')
  }
  if(!all(names(daytype)[2:ncol(daytype)] == as.character(1:(ncol(daytype)-1)))){
    stop(paste0("Problem in ts.txt chroniques names actual : ",
                paste0(names(daytype)[2:ncol(daytype)], collapse = ","),
                "  ||  Must be",
                paste0(as.character(1:(ncol(daytype)-1)), collapse = ",")))
  }
  
  daytype[, Date := as.character(Date)]
  dateControl <- strsplit(daytype$Date[1], "-")[[1]]
  if(length(dateControl)!=3){
    stop(paste0("Problem of date format in file ts.txt, must be AAAA-MM-DD, actual : ", 
                daytype$Date[1]))
  }
  
  daytype
}

