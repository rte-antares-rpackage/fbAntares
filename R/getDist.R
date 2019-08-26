#' @title Adjust a polyhedron from a set of facets to approach a reference polyhedron
#' 
#' @description This function takes in input two polyhedrons, the reference one and
#' the moving one (or set of facets). It returns in output a really close polyhedron,
#' to the reference one, with the set of facets. It runs a minimization program 
#' on a distance using the intersections of a set of lines and the two polyhedrons 
#' and validate the moving polyhedron with a volume indicator.
#' 
#' @param A \code{data.table}, fix polyhedron, data.table containing at least 
#' ram, Date, Period and two ptdf columns :
#' \itemize{
#'  \item ptdfAT : autrichian ptdf
#'  \item ptdfBE : belgium ptdf
#'  \item ptdfDE : german ptdf
#'  \item ptdfFR : french ptdf
#'  \item ram : line limits
#'  \item Date : date in format YYYY-MM-DD
#'  \item Period : hour in the day, between 1 and 24
#' }
#' @param B \code{data.table}, moving polyhedron, data.table containing at least 
#' ram, Date, Period and two ptdf columns :
#' \itemize{
#'  \item ptdfAT : autrichian ptdf
#'  \item ptdfBE : belgium ptdf
#'  \item ptdfDE : german ptdf
#'  \item ptdfFR : french ptdf
#'  \item ram : line limits
#'  \item Date : date in format YYYY-MM-DD
#'  \item Period : hour in the day, between 1 and 24
#' }
#' @param nbLines \code{numeric}, number of half-lines used to compute the distance
#' between the two polyhedra.
#' @param maxiter \code{numeric}, maximum number of iteration, it is one of the 
#' two stopping criteria of this function.
#' @param thresholdIndic \code{numeric}, maximum number of iteration, it is one of the 
#' two stopping criteria of this function.
#' @param quad \code{logical}, TRUE if you want to solve it with a quadratic
#' optimization problem, FALSE if you want to use a linear (default is linear, 
#' which is faster)
#' @param seed \code{numeric} fixed random seed, used for the weighted draw of the 
#' typical days. By default, the value is 123456
#' @param verbose \code{numeric}, shows the logs in console. By default, the value is 1.
#' \itemize{
#'  \item 0 : No log
#'  \item 1 : No log
#'  \item 2 : Medium log
#'  }
#' @param fixFaces \code{data.table} data.table if you want to use fix faces for the creation
#' of the flowbased models. If you want to do it, the data.table has the following form :
#' data.table(func = c("min", "min", "max", "min"), zone = c("BE", "FR", "DE", "DE")).
#' func is the direction of the fix faces and zone is the area of this direction.
#' If you give for example min and DE, there will be a fix face at the minimum import
#' value of Germany.
#' @param VERTRawDetails \code{data.table}, vertices of the polyhedron A
#' @examples
#' \dontrun{
#' library(data.table)
#' library(quadprog)
#' library(linprog)
#' polyhedra <- readRDS(system.file("testdata/polyhedra.rds", package = "fbAntares"))
#' A <- polyhedra[Date == "2019-02-14"]
#' B <- polyhedra[Date == "2019-02-15"]
#' nbLines <- 10000
#' maxiter <- 20
#' thresholdIndic <- 0.9
#' quad <- F
#' 
#'  getBestPolyhedron(
#'  A = A, B = B, nbLines = nbLines, maxiter = maxiter,
#'  thresholdIndic = thresholdIndic, quad = quad)
#' }
#' 
#' @import data.table 
#' @import quadprog
#' @import linprog
#' @import lpSolve
#' @importFrom stats rnorm runif
#' 
#' @export

getBestPolyhedron <- function(A, B, nbLines, maxiter, thresholdIndic, quad = F, 
                              verbose = 2, seed = 123456, fixFaces, VERTRawDetails) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  Line_Coo_X1 <- NULL
  Line_Coo_X2 <- NULL
  
  col_ptdf <-  .crtlPtdf(A, B)
  .crtldtFormat(A)
  .crtldtFormat(B)
  .crtlNumeric(nbLines)
  .crtlNumeric(maxiter)
  .crtlNumeric(thresholdIndic)
  .crtlBoolean(quad)
  # browser()
  A <- copy(A)
  # Generation of the lines for the optimization
  dtLines <- .getNormalizedLines(nbLines = nbLines, dim = length(col_ptdf))
  
  ###########
  ## Saving of the extremas of A, i.e max import and export
  ## of the real model in order to approach it in a better way with the
  ## modelized one
  maxRow <- unname(unlist(VERTRawDetails[, lapply(
    .SD, which.max), .SDcols = colnames(
      VERTRawDetails)[!grepl("idDayType|Date|Period", colnames(VERTRawDetails))]]))
  minRow <- unname(unlist(VERTRawDetails[, lapply(
    .SD, which.min), .SDcols = colnames(
      VERTRawDetails)[!grepl("idDayType|Date|Period", colnames(VERTRawDetails))]]))
  
  # On écrit les droites qui passent par ces extremas
  normvec <- sqrt(rowSums(VERTRawDetails[
    c(minRow, maxRow), .SD, .SDcols = colnames(
      VERTRawDetails)[!grepl("idDayType|Date|Period", colnames(VERTRawDetails))]]^2))
  
  Linesmerge <- VERTRawDetails[c(minRow, maxRow), .SD, .SDcols = colnames(
    VERTRawDetails)[!grepl("idDayType|Date|Period", colnames(VERTRawDetails))]]/normvec
  setnames(Linesmerge, colnames(dtLines)[grep("Line", colnames(dtLines))])
  

  # Adding of new lines (10% of the generated lines)
  # noising of them to not have only equal lines
  Linesmerge <- rbindlist(lapply(1:(nbLines/(10*nrow(Linesmerge))), function(X) {
    Linesmerge + runif(nrow(Linesmerge))/100000
  }))
  
  dtLines <- merge(dtLines, Linesmerge, by = colnames(Linesmerge), all = T)
  
  # Computation of the intersectionss between lines and real domain
  fixPlan <- .getIntersecPoints(dtLines, A)
  PLANOUT <- copy(B)
  # Ordering of fixPlan
  fixPlan <- fixPlan[order(Line_Coo_X1, Line_Coo_X2)]
  # Computation of the intersections between lines and modelized domain
  movingPlan <- .getIntersecPoints(dtLines, PLANOUT)
  movingPlan <- movingPlan[order(Line_Coo_X1, Line_Coo_X2)]
  indic0 <- 0
  for(k in 1:maxiter) {
    ## Call of optimization function
    newram <- .getDmatdvec(fixPlan, movingPlan, PLANOUT, col_ptdf, quad, fixFaces)
    for (i in 1:nrow(PLANOUT)) {
      ## If a face is intersected with lines, we change its ram value
      if(newram[i] != 0) {
        PLANOUT$ram[i] <- newram[i]
      }
    }
    # Value of volume intra inter
    indic <- evalInter(PLANOUT, A)[1, 1]
    
    if (verbose > 1) {
      print(paste("Iteration", k, "indic :", indic))
    }
    # stopping criteria
    if (indic > indic0) {
      indic0 <- indic
      bestRam <- newram
    } else if (indic == indic0) {
      return(PLANOUT)
    }else if (indic == indicmoinsun) {
      return(PLANOUT)
    }
    movingPlan <- .getIntersecPoints(dtLines, PLANOUT)
    movingPlan <- movingPlan[order(Line_Coo_X1, Line_Coo_X2)]
    if (indic > thresholdIndic) {
      return(PLANOUT)
    } 
    indicmoinsun <- indic
    
  }
  return(PLANOUT)
}

#######

.getDmatdvec <- function(fixPlan, movingPlan, PLANOUT, col_ptdf, quad, fixFaces) {
  
  Face <- NULL
  ## Optimization part, creation of the matrices needed in the program
  dvec <- rep(0, length(PLANOUT$Face))
  Dmat <- rep(0, length(PLANOUT$Face))
  for(DD  in unique(PLANOUT$Face)){
    Fb <- PLANOUT[Face == DD]
    # face by face, we check which are concerned in fixPlan and movingPlan
    RO <- which(movingPlan$Face == DD)
    movingPlan2 <- movingPlan[RO]
    fixPlan2 <- fixPlan[RO]
    colnames(fixPlan2)[grep("^X[1-9]", colnames(fixPlan2))]
    # intersection points
    ak <- as.matrix(fixPlan2[, .SD, .SDcols = colnames(fixPlan2)[
      grep("^X[1-9]", colnames(fixPlan2))]])
    # normal to hyperplanes
    Nai <- as.matrix(fixPlan2[, .SD, .SDcols = col_ptdf])
    # bk <- as.matrix(movingPlan2[, .SD, .SDcols = colnames(fixPlan2)[
    #   grep("^X[1-9]", colnames(movingPlan2))]])
    
    
    if(length(RO)>1){
      # same procedure with movingPlan
      colnames(movingPlan2)[
        grep("^Line_Coo_X[1-9]", colnames(fixPlan2))]
      uk <- as.matrix(movingPlan2[, .SD, .SDcols = colnames(movingPlan2)[
        grep("^Line_Coo_X[1-9]", colnames(fixPlan2))]])
      Nbh <- as.vector(unlist(Fb[, .SD, .SDcols = col_ptdf]))
      Nbh <- rep(Nbh,nrow(Nai))
      Nbh <- matrix(Nbh, nrow = nrow(Nai), byrow = T)
      
      ukN <- rowSums(uk*Nbh)
      # ukN <- matrix(rep(ukN, ncol(uk)), ncol = ncol(uk))
      ukN <- uk / ukN
      
      dvec[DD] <- sum(fixPlan2$ram + rowSums(ak * (uk-Nai)))
      Dmat[DD] <- sum(1 + rowSums(ukN * (uk - Nbh)))
      
    }
  }
  # Optimization launching
  .launchOptim(Dmat, dvec, quad, fixFaces, PLANOUT)
}


.launchOptim <- function(Dmat, dvec, quad, fixFaces, PLANOUT) {
  
  if (quad) {
    # quadratic program
    Dmat[Dmat == 0] <- 1
    Dmat <- diag(Dmat, nrow = length(Dmat))
    Amat <- diag(1, nrow = nrow(Dmat))
    bvec <- rep(0, nrow(Dmat))
    res <- solve.QP(Dmat = Dmat,
                    dvec = dvec,
                    Amat = Amat,
                    bvec = bvec)
  } else {
    # linear program
    Bdiag <- diag(Dmat, nrow =length(Dmat))
    if (!is.null(fixFaces)) {
      if (nrow(fixFaces) > 0) {
        ## in order not to change the ram of the fix faces
        if (nrow(fixFaces) > 1) {
          diag(Bdiag[(ncol(Bdiag)-nrow(fixFaces)+1):ncol(Bdiag),
                     (ncol(Bdiag)-nrow(fixFaces)+1):ncol(Bdiag)]) <- 1
        } else {
          Bdiag[(ncol(Bdiag)-nrow(fixFaces)+1):ncol(Bdiag),
                (ncol(Bdiag)-nrow(fixFaces)+1):ncol(Bdiag)] <- 1
        }
        
        dvec[(length(dvec)-nrow(fixFaces)+1):length(dvec)] <- PLANOUT$ram[
          (nrow(PLANOUT)-nrow(fixFaces)+1):nrow(PLANOUT)] 
      }
    }
    ## Optimization launching
    res <- lp(direction = "min", objective.in = Dmat, const.mat = Bdiag, 
              const.rhs = dvec, const.dir = ">=")
  }
  ## new values of ram
  hbhEvol <- round(res$solution)
  hbhEvol
}
