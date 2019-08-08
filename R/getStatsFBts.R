#' @title Return a list with statistics summary of the file ts.txt
#' 
#' @description 
#' This function takes the object ts, generated with \link{createFBTS}
#' and return a list with statistics summary of this object
#'
#' @param ts \code{data.table} The output ts obtained with \link{createFBTS}
#' @param interSeasonBegin \code{character or date}, date or vector of dates, YYYY-MM-DD, 
#' begin of interseason
#' @param interSeasonEnd \code{character or date}, date or vector of dates, YYYY-MM-DD, 
#' end of interseason
#' @param output \code{character}, the output of you want, either summary, 
#' yearbyyear or all, default is summary, if you want the details check the example.
#' 
#' @examples
#' \dontrun{
#' ts <- fread(system.file("testdata/antaresInput/ts.txt", package = "fbAntares"),
#' header = T)
#' interSeasonBegin <-  c("2028-10-01", "2029-03-16")
#' interSeasonEnd <- c("2028-10-31", "2029-05-15")
#' statsFBts <- getStatsFBts(ts, interSeasonBegin, interSeasonEnd, output = "summary")
#' statsFBts <- getStatsFBts(ts, interSeasonBegin, interSeasonEnd, output = "yearbyyear")
#' statsFBts <- getStatsFBts(ts, interSeasonBegin, interSeasonEnd, output = "all")
#' }
#' @importFrom stats median sd var
#' @export

getStatsFBts <- function(ts, interSeasonBegin =  c("2028-10-01", "2029-03-16"), 
                         interSeasonEnd = c("2028-10-31", "2029-05-15"),
                         output = "summary") {
  setDT(ts)
  minDate <- as.Date(ts[, min(Date)])
  firstDay <- wday(minDate)
  if (firstDay == 1) {
    firstDay <- 7
  } else {
    firstDay <- firstDay - 1
  }
  dates <- unique(ts$Date)
  calendar <- .getVirtualCalendar(dates, interSeasonBegin, interSeasonEnd, firstDay)
  
  namesWe <- names(calendar)[grepl("We", names(calendar))]
  
  ts[, Date := as.Date(Date)]
  calendar <- lapply(calendar, as.Date)
  
  .checkDayInTS(ts, calendar)
  
  setDF(ts)
  for (X in 2:ncol(ts)) {
    ts[, X] <- factor(ts[, X], levels = 1:12)
  }
  typicalDayByYear <- rbindlist(lapply(2:ncol(ts), function(X) {
    summary <- summary(ts[, X])
    data.table(
      ClimateYear = (X-1), 
      dayType1 = summary[[1]], dayType2 = summary[[2]], dayType3 = summary[[3]],
      dayType4 = summary[[4]], dayType5 = summary[[5]], dayType6 = summary[[6]],
      dayType7 = summary[[7]], dayType8 = summary[[8]], dayType9 = summary[[9]],
      dayType10 = summary[[10]], dayType11 = summary[[11]], dayType12 = summary[[12]])
  }))
  setDT(ts)
  typDayBySeason <- rbindlist(lapply(2:ncol(typicalDayByYear), function(X) {
    if(X %in% 2:4) {
      typDay <- "summerWd"
    } else if(X == 5) {
      typDay <- "summerWe"
    } else if(X %in% c(6:8)) {
      typDay <- "winterWd"
    } else if(X == 9) {
      typDay <- "winterWe"
    } else if(X %in% 10:12) {
      typDay <- "interSeasonWd"
    } else if(X == 13) {
      typDay <- "interSeasonWe"
    }
    
    typicalDayByYear[, list(
      typDaySeason = typDay, typicalDay = X-1, 
      mean = round(mean(get(paste0("dayType", (X-1)))), 2), 
      median = round(median(get(paste0("dayType", (X-1)))), 2),
      sd = round(sd(get(paste0("dayType", (X-1)))), 2),
      var = round(var(get(paste0("dayType", (X-1)))), 2),
      min = round(min(get(paste0("dayType", (X-1)))), 2),
      max = round(max(get(paste0("dayType", (X-1)))), 2))]
  }))
  
  if (output == "summary") {
    return(typDayBySeason)
  } else if (output == "yearbyyear") {
    return(typicalDayByYear)
  } else if (output == "all") {
    return(list(typicalDayByYear = typicalDayByYear, typDayBySeason = typDayBySeason))
  } else {
    stop(paste("The output can only be summary, yearbyyear or all, currently :", output))
  }
  
  
}