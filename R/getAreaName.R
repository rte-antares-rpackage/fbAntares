.getAreaName <- function(areaName){
  dd <- system.file("areaName/areaName.csv", package = "fbAntares")
  dd <- fread(dd)
  if(!areaName%in%dd$area){
    stop("areaName not found in conf file")
  }
  
  dd <- dd[area == areaName]
  dd$links <- list(strsplit(dd$links, ","))
  dd$country <- list(strsplit(dd$country, ","))
  
  exp <- strsplit(dd$antares, "[,]")[[1]]
  dd$antares <- list(list(sapply(exp, function(X){strsplit(X, "[|]")})))
  
  dd
}
