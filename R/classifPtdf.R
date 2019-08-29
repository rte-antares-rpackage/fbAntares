#' @title Give B from PTDF
#' 
#' @description Make a clustering from the ptdf equations in order to get
#' a set of ptdf to approximate all the typical days' polyhedra
#'
#' @param PTDF \code{data.frame | data.table}, PTDF
#' @param nbClust \code{numeric}, number of cluster
#' @importFrom stats kmeans
#' @importFrom utils read.table
#' @noRd
#' 
giveBClassif <- function(PTDF, nbClust = 36, fixFaces, col_ptdf, 
                         clusteringDayType, clusteringHours)
{
  addFixFaces <- zone <- Ind <- NULL
  # select only the hours and idDayType you want to use to obtain the
  # final faces
  if(length(clusteringDayType) == 1) {
    if (clusteringDayType == "All") {
      clusteringDayType <- unique(PTDF$idDayType)
    }
  }
  if(length(clusteringHours) == 1) {
    if (clusteringHours == "All") {
      clusteringHours <- unique(PTDF$Period)
    }
  }
  # keep only column to cluster
  PTDFKm <- PTDF[Period %in% clusteringHours & idDayType %in% clusteringDayType,
                 .SD, .SDcols = colnames(PTDF)[grep("ptdf", colnames(PTDF))]]
  
  # normalize the values in order to make the clustering
  PTDFKmCare <- PTDFKm^2
  PTDFKmCare <- rowSums(PTDFKmCare)
  PTDFKm <- PTDFKm / sqrt(PTDFKmCare)
  # in order not to overload the ram
  # if not too much lines -> CAH, else -> kmeans + CAH
  if (nrow(PTDFKm)^2 < 2^28) {
    res <- cutree(hclust(dist(PTDFKm, method = "euclidean"), 
                         method = "ward.D"), nbClust)
  } else {
    
    resKm <- kmeans(PTDFKm, centers = 5000, nstart = 20)
    
    res <- cutree(hclust(dist(resKm$centers, method = "euclidean"), 
                         method = "ward.D"), nbClust)
    
    dtresKm <- data.table(Ind =1:length(resKm$cluster) ,resKm = resKm$cluster)
    dtresCah <- data.table(resKm = as.numeric(names(res)), res)
    dtres <- merge(dtresKm, dtresCah, by = "resKm")
    dtres <- dtres[order(Ind)]
    res <- dtres$res
  }
  # keep the centers of the clusters as faces for the modelized polyhedra
  PTDFKm$clust <- res
  centers <- PTDFKm[,lapply(.SD, mean), by = "clust"]
  centers <- centers[, .SD, .SDcols = colnames(centers)[
    grep("ptdf", colnames(centers))]]
  
  
  # adding of the fix faces if they exist
  addFixFaces <- function(centers, fixFaces) {
    # browser()
    centers <- rbindlist(list(centers, rbindlist(lapply(1:nrow(fixFaces), function(X) {
      # browser()
      # which area has the fix face
      ptdfnotnull <- col_ptdf[grepl(fixFaces[X, zone], col_ptdf)]
      ptdfnull <- col_ptdf[!grepl(fixFaces[X, zone], col_ptdf)]
      func <- fixFaces[X, func]
      # is it max import or export
      valfunc <- ifelse(func == "min", -1, 1)
      
      dt <- read.table(text = "", col.names = c(ptdfnotnull, ptdfnull))
      dt[1, ] <- c(valfunc, rep(0, length(ptdfnull)))
      setDT(dt)
      setcolorder(dt, colnames(centers))
      dt
    }))))
  }
  if (!is.null(fixFaces)) {
    if (nrow(fixFaces) >= 1) {
      centers <- addFixFaces(centers = centers, fixFaces = fixFaces)
    }
  }
  centers

}
