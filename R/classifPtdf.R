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
  
  res <- cutree(hclust(dist(PTDFKm, method = "euclidean"), method = "ward.D"), nbClust)
  
  PTDFKm$clust <- res
  centers <- PTDFKm[,lapply(.SD, mean), by = "clust"]
  centers <- centers[, .SD, .SDcols = colnames(centers)[grep("ptdf", colnames(centers))]]

  
  # affectRow <- function(centers, valueVect)
  # {
  #   conCernRow <- which.min(colSums((t(as.matrix(centers[, .SD, .SDcols = c("BE", "DE", "FR")]))-c(valueVect))^2))
  #   centers[conCernRow,c("BE", "DE","FR"):=as.list(valueVect)]
  # }
  # affectRow(centers, c(-1,0,0))
  # affectRow(centers, c(0,-1,0))
  # affectRow(centers, c(0,0,-1))
  # affectRow(centers, c(0,1,0))
  # centers[,c("BE", "DE", "FR")]
}
