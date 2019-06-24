.getNormalizedLines <- function(nbLines, dim) {
  
  # dtLines <- data.table(Line = 1:nbLines)
  dtLines <- data.table(Line_Coo_X1 = rnorm(n = nbLines))
  for (i in 1:dim) {
    dtLines[, paste0("Line_Coo_X", i) := rnorm(n = nbLines)]
  }
  dtLines[, normvec := sqrt(rowSums(dtLines[, 1:dim]^2))]
  for (i in 1:dim) {
    dtLines[, paste0("Line_Coo_X", i) := get(paste0("Line_Coo_X", i))/normvec]
  }
  # dtLines[, normvec := sqrt(rowSums(dtLines[, 1:dim]^2))]
  dtLines[, normvec := NULL]
  return(dtLines)
}




.getIntersecPoints <- function(dtLines, PLAN) {
  PLAN[, Face := 1:nrow(PLAN)]
  # Passage en matrice des coordonnées des vecteurs directeurs des droites
  matLines <- as.matrix(dtLines[, .SD, .SDcols = colnames(dtLines)[
    grep("Line_Coo_X", colnames(dtLines))]], nrow = nrow(dtLines))
  # On récupère les colonnes ptdf
  col_ptdf <- colnames(PLAN)[grep("ptdf", colnames(PLAN))]
  ptdf <- matrix(unname(unlist(
    PLAN[, .SD, .SDcols = col_ptdf])), nrow = length(col_ptdf), byrow = T)
  ram <- unname(unlist(PLAN[, .SD, .SDcols = "ram"]))
  
  # Valeur du produit scalaire dans la formule pour trouver le lambda
  denom <- matLines %*% ptdf
  
  # surcharge de ram si tout d'un coup
  lambda <- t(ram/t(denom))
  # surcharge de ram si tout d'un coup
  lambda[lambda<0] <- 10000000
  
  pct1 <- trunc(nrow(matLines)/100)
  
  Points <- apply(lambda, 1, function(X)which.min(abs(X)))
  lambdaout<- apply(lambda, 1, function(X)min(abs(X)))
  
  # all(toto == Points$Face)
  
  # Points[, Line := rep(1:nrow(Points))]
  Points <- data.table(Face = Points)
  Points$lambda <- lambdaout
  
  for(i in 1:nrow(ptdf)) {
    
    Points[[paste0("Line_Coo_X", i)]] <- matLines[, paste0("Line_Coo_X", i)]
    
    Points[[paste0("X", i)]] <- Points$lambda*matLines[, paste0("Line_Coo_X", i)]
    
  }
  
  Points <- merge(Points, PLAN, by = "Face")
  Points$distOrig <- abs(Points$lambda)
  Points
}


evalInter <- function(P1, P2, nbPoints = 50000){
  
  col_ptdf <- colnames(P1)[grep("ptdf", colnames(P1))]
  last_ptdfcol <- col_ptdf[length(col_ptdf)]
  
  # Voir peut-être comment rendre ça plus propre
  PT <- data.table(Line_Coo_X1 = runif(nbPoints) * 30000 - 15000)
  for (i in 2:(length(col_ptdf)-1)) {
    PT[, paste0("Line_Coo_X", i) := runif(nbPoints) * 30000 - 15000]
  }

  col_ptdf <- colnames(P1)[grep("ptdf", colnames(P1))]
  last_ptdfcol <- col_ptdf[length(col_ptdf)]
  
  clcPTin <- function(P, PT, col_ptdf){
    for (col in col_ptdf[1:(length(col_ptdf)-1)]) {
      P[[col]] <- P[[col]] - P[[last_ptdfcol]]
    }
    
    re <- as.matrix(P[, .SD, .SDcols = col_ptdf[1:(length(col_ptdf)-1)]])%*%t(PT)
    which(apply(re, 2, function(X, Y){all(X<Y)}, Y = P$ram))
  }
  
  indomaine1 <- clcPTin(P1, PT, col_ptdf)
  indomaine2 <- clcPTin(P2, PT, col_ptdf)
  
  return(length(intersect(indomaine1, indomaine2))/length(union(indomaine1, indomaine2)))
  
}
