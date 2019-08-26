.getNormalizedLines <- function(nbLines, dim) {
  ## Input : number of lines to compute and space-dimension of the lines
  ## Output : data.table with lines details (with norm == 1)
  
  normvec <- NULL
  # control function
  .crtlgetNormalizedLines(nbLines = nbLines, dim = dim)
  dtLines <- data.table(Line_Coo_X1 = rnorm(n = nbLines))
  
  # We generate the lines using normal distribution with dimension equal
  # to the number of ptdf
  for (i in 1:dim) {
    dtLines[, paste0("Line_Coo_X", i) := rnorm(n = nbLines)]
  }
  # lines normalization
  dtLines[, normvec := sqrt(rowSums(dtLines[, 1:dim]^2))]
  for (i in 1:dim) {
    dtLines[, paste0("Line_Coo_X", i) := get(paste0("Line_Coo_X", i))/normvec]
  }
  dtLines[, normvec := NULL]
  return(dtLines)
}




.getIntersecPoints <- function(dtLines, PLAN, center = NULL) {
  ## Input : 
  ##  dtLines, obtained with the previous function .getNormalizedLines
  ##  PLAN : data.table containing ptdf and ram of a polyhedron
  ## Output : data.table containing the intersections of the lines and the polyhedron
  

  Face <- NULL
  .crtldtFormat(dtLines)
  .crtldtFormat(PLAN)
  
  PLAN[, Face := 1:nrow(PLAN)]
  # transformation of the coordinates of the directory vectors of the lines into
  # matrices
  matLines <- as.matrix(dtLines[, .SD, .SDcols = colnames(dtLines)[
    grep("Line_Coo_X", colnames(dtLines))]], nrow = nrow(dtLines))
  
  # getting ptdf colnames
  col_ptdf <-  .crtlPtdf(PLAN)
  ptdf <- matrix(unname(unlist(
    PLAN[, .SD, .SDcols = col_ptdf])), nrow = length(col_ptdf), byrow = T)
  ram <- unname(unlist(PLAN[, .SD, .SDcols = "ram"]))
  
  # value of the scalar product in the optimization formula
  denom <- matLines %*% ptdf
  
  ######## Test on the center
  if (!is.null(center)) {
    center <- as.matrix(center)
    center <- matrix(center, nrow = ncol(ptdf), ncol = 4, byrow = T)
    # browser()
    tCenter <- colSums(ptdf * t(center))
    denom2 <- tCenter/t(denom)
  }
  # surcharge de ram si tout d'un coup
  lambda <- t(ram/t(denom)) 
  if (!is.null(center)) {
    lambda <- lambda - t(denom2)
  }
  
  # in order to have an orientation of the vectors
  lambda[lambda<0] <- 10000000
  
  # writting of the intersection points
  Points <- apply(lambda, 1, function(X)which.min(abs(X)))
  lambdaout<- apply(lambda, 1, function(X)min(abs(X)))
  
  Points <- data.table(Face = Points)
  # lambda is the euclidian distance to the origin
  Points$lambda <- lambdaout
  center <- unique(center)
  for(i in 1:nrow(ptdf)) {
    
    Points[[paste0("Line_Coo_X", i)]] <- matLines[, paste0("Line_Coo_X", i)]
    if (is.null(center)) {
      Points[[paste0("X", i)]] <- Points$lambda*matLines[, paste0("Line_Coo_X", i)]
    } else {
      Points[[paste0("X", i)]] <- Points$lambda*matLines[, paste0("Line_Coo_X", i)] + center[1, i]
    }
    
  }
  # finally we get in output the intersection points in data.table format 
  Points <- merge(Points, PLAN, by = "Face")
  Points$distOrig <- abs(Points$lambda)
  Points
}

#' @title Evaluate the shared volume of two polyhedra A and B
#' 
#' @description This function returns and indicator between 0 and 1 where 0 means
#' the intersection between A and B is empty and 1 means A == B.
#' Since there is no fast metric to compute the volume of a polyhedron in high 
#' dimension, this indicator is computed by generating n points in a n-dimensonal
#' space doing the ratio of the points in the two polyhedra and the points in
#' at least one of the polyhedra.
#' 
#' @param A \code{data.table}, fix polyhedron, data.table containing at least 
#' two ptdf columns :
#' \itemize{
#'  \item ptdfAT : autrichian vertices
#'  \item ptdfBE : belgium vertices
#'  \item ptdfDE : german vertices
#'  \item ptdfFR : french vertices
#' }
#' @param B \code{data.table}, moving polyhedron, data.table containing at least 
#' two ptdf columns :
#' \itemize{
#'  \item ptdfAT : autrichian vertices
#'  \item ptdfBE : belgium vertices
#'  \item ptdfDE : german vertices
#'  \item ptdfFR : french vertices
#' }
#' @param nbPoints \code{numeric}, number of points generated
#' @param seed \code{numeric} fixed random seed, used for the weighted draw of the 
#' typical days. By default, the value is 123456
#' 
#' @examples
#' \dontrun{
#' library(data.table)
#' polyhedra <- readRDS(system.file("testdata/polyhedra.rds", package = "fbAntares"))
#' A <- polyhedra[Date == "2019-02-14"]
#' B <- polyhedra[Date == "2019-02-15"]
#' nbPoints <- 50000
#' 
#'  evalInter(A = A, B = B, nbPoints = nbPoints)
#' }
#' 
#' @import data.table 
#' @importFrom stats runif
#' 
#' @export
evalInter <- function(A, B, nbPoints = 50000, seed = 123456){
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  .crtldtFormat(A)
  .crtldtFormat(B)
  .crtlNumeric(nbPoints)
  col_ptdf <-  .crtlPtdf(A, B)
  last_ptdfcol <- col_ptdf[length(col_ptdf)]
  
  # Voir peut-être comment rendre ça plus propre
  PT <- data.table(Line_Coo_X1 = runif(nbPoints) * 30000 - 15000)
  for (i in 2:(length(col_ptdf)-1)) {
    PT[, paste0("Line_Coo_X", i) := runif(nbPoints) * 30000 - 15000]
  }
  
  
  clcPTin <- function(P, PT, col_ptdf){
    for (col in col_ptdf[1:(length(col_ptdf)-1)]) {
      P[[col]] <- P[[col]] - P[[last_ptdfcol]]
    }
    
    re <- as.matrix(P[, .SD, .SDcols = col_ptdf[1:(length(col_ptdf)-1)]])%*%t(PT)
    which(apply(re, 2, function(X, Y){all(X<Y)}, Y = P$ram))
  }
  
  indomaine1 <- clcPTin(A, PT, col_ptdf)
  indomaine2 <- clcPTin(B, PT, col_ptdf)
  
  volIntraInter <- (length(intersect(indomaine1, indomaine2))/length(union(indomaine1, indomaine2)))*100
  error1 <- (1-length(intersect(indomaine1, indomaine2))/length(indomaine1))*100
  error2 <- (1-length(intersect(indomaine1, indomaine2))/length(indomaine2))*100
  
  return(data.frame(volIntraInter = volIntraInter, error1 = error1, error2 = error2))
  
}
