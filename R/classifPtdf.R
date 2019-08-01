#' @title Give B from PTDF
#' 
#' @description Make a clustering from the ptdf equations in order to get
#' a set of ptdf to approximate all the typical days' polyhedra
#'
#' @param PTDF \code{data.frame | data.table}, PTDF
#' @param nbClust \code{numeric}, number of cluster
#'
#' @noRd
#' 
giveBClassif <- function(PTDF, nbClust = 36)
{
  
  PTDFKm <- PTDF[, .SD, .SDcols = colnames(PTDF)[grep("ptdf", colnames(PTDF))]]
  
  # normalize the values in order to make the clustering
  PTDFKmCare <- PTDFKm^2
  PTDFKmCare <- rowSums(PTDFKmCare)
  PTDFKm <- PTDFKm / sqrt(PTDFKmCare)
  if (nrow(PTDFKm)^2 < 2^30) {
    res <- cutree(hclust(dist(PTDFKm, method = "euclidean"), method = "ward.D"), nbClust)
  } else {
    
    resKm <- kmeans(PTDFKm, centers = 5000, nstart = 10)
    
    res <- cutree(hclust(dist(resKm$centers, method = "euclidean"), method = "ward.D"), nbClust)
    
    dtresKm <- data.table(Ind =1:length(resKm$cluster) ,resKm = resKm$cluster)
    dtresCah <- data.table(resKm = as.numeric(names(res)), res)
    dtres <- merge(dtresKm, dtresCah, by = "resKm")
    dtres <- dtres[order(Ind)]
    res <- dtres$res
  }
  
  PTDFKm$clust <- res
  centers <- PTDFKm[,lapply(.SD, mean), by = "clust"]
  centers <- centers[, .SD, .SDcols = colnames(centers)[grep("ptdf", colnames(centers))]]
  
  
  
  ##### Explication nécessaire de la partie affectRow et de l'intérêt des faces fixes
  ##### car de meilleurs résultats sans l'utiliser
  affectRow <- function(centers, valueVect)
  {
    conCernRow <- which.min(colSums((t(as.matrix(centers[, .SD, .SDcols = paste0(
      "ptdf", c("BE", "DE", "FR", "AT"))])) - c(valueVect))^2))
    centers[conCernRow,  paste0(
      "ptdf", c("BE", "DE", "FR", "AT")) := as.list(valueVect)]
  }
  # affectRow(centers, c(-1,0,0,0))
  # affectRow(centers, c(0,-1,0,0))
  # affectRow(centers, c(0,0,-1,0))
  # affectRow(centers, c(0,1,0,0))
  centers[,paste0("ptdf", c("BE", "DE", "FR", "AT"))]
}
