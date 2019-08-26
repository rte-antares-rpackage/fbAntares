.getAreaName <- function(areaName){
  # read the csv of the package
  dd <- system.file("areaName/areaName.csv", package = "fbAntares")
  dd <- fread(dd)
  if(!areaName %in% dd$area){
    stop("areaName not found in conf file")
  }
  # keep only the area type we want
  dd <- dd[area == areaName]
  # write the links
  dd$links <- list(strsplit(dd$links, ","))
  dd$country <- list(strsplit(dd$country, ","))
  # splitting to obtain how the ares work
  exp <- strsplit(dd$antares, "[,]")[[1]]
  dd$antares <- list(list(sapply(exp, function(X){strsplit(X, "[|]")})))
  
  dd
}
