#' @title Draw a volume heatmap comparing the volumes of two polyhedrons
#' 
#' @description This function returns a heatmap comparing the volumes of two polyhedrons
#' in different directions corresponding to the positive/negative net positions of
#' each country
#' 
#' @param A \code{data.table}, polyhedron, data.table containing at least 
#' two ptdf columns :
#' \itemize{
#'  \item ptdfAT : autrichian vertices
#'  \item ptdfBE : belgium vertices
#'  \item ptdfDE : german vertices
#'  \item ptdfFR : french vertices
#' }
#' @param B \code{data.table}, polyhedron, data.table containing at least 
#' two ptdf columns :
#' \itemize{
#'  \item ptdfAT : autrichian vertices
#'  \item ptdfBE : belgium vertices
#'  \item ptdfDE : german vertices
#'  \item ptdfFR : french vertices
#' }
#' If B is NULL, the heatmap returns the absolute volume of A in the different directions.
#' @param nbPoints \code{numeric}, number of points generated for volume comparison
#' @param seed \code{numeric} fixed random seed, used for the weighted draw of the
#' points for volume assessment. By default, the value is 123456
#' @param assessment_range The range in which the points for the volume assessment should be drawn
#' @examples
#' \dontrun{
#' library(data.table)
#' polyhedra <- readRDS(system.file("testdata/polyhedra.rds", package = "fbAntares"))
#' A <- polyhedra[Date == "2019-02-14"]
#' B <- polyhedra[Date == "2019-02-15"]
#' nbPoints <- 50000
#' 
#'  drawVolumeHeatmap(A = A, B = B, nbPoints = nbPoints)
#' }
#' 
#' @import data.table
#' @importFrom plotly plot_ly
#' @import stringr
#' 
#' @export

drawVolumeHeatmap <- function(A, B = NULL, nbPoints = 50000, seed = 123456, assessment_range = c(-15000, 15000)){
  
  col_ptdf <-  .crtlPtdf(A, B)
  
  heatmap <- sapply(col_ptdf[-length(col_ptdf)], function(X){
    
    current_country <- str_remove(X, "ptdf")
    volume_positive_A <- evalDomainVolume(A, nbPoints, seed, assessment_range, direction = data.table(country = current_country, direction = "positive"))
    volume_negative_A <- evalDomainVolume(A, nbPoints, seed, assessment_range, direction = data.table(country = current_country, direction = "negative"))
    
    if(!is.null(B)){
      volume_positive_B <- evalDomainVolume(B, nbPoints, seed, assessment_range, direction = data.table(country = current_country, direction = "positive"))
      volume_negative_B <- evalDomainVolume(B, nbPoints, seed, assessment_range, direction = data.table(country = current_country, direction = "negative"))
      
      diff_volume_positive <- (volume_positive_B - volume_positive_A) / volume_positive_A * 100
      diff_volume_negative <- (volume_negative_B - volume_negative_A) / volume_negative_A * 100
      
      c(diff_volume_positive, diff_volume_negative)
    } else{
      c(volume_positive_A, volume_negative_A)
    }
    
  })
  
  colnames(heatmap) <- sapply(col_ptdf[-length(col_ptdf)], str_remove, "ptdf")
  rownames(heatmap) <- c("positive", "negative")
  
  colorlist <- .returnColors(c(heatmap))
  colorlist[, vals := scales::rescale(vals)]
  
  fig <- plot_ly(x = colnames(heatmap), y = rownames(heatmap), z = heatmap, colorscale = colorlist, type = "heatmap")
  fig
  
}

#' @title Draw a volume heatmap comparing the volumes of two polyhedrons making combinations
#' of possible net positions for the studied countries
#' 
#' @description This function returns a heatmap comparing the volumes of two polyhedrons
#' in different directions corresponding to all the combinations of the positive/negative
#' net positions of each country
#' 
#' @param A \code{data.table}, polyhedron, data.table containing at least 
#' two ptdf columns :
#' \itemize{
#'  \item ptdfAT : autrichian vertices
#'  \item ptdfBE : belgium vertices
#'  \item ptdfDE : german vertices
#'  \item ptdfFR : french vertices
#' }
#' @param B \code{data.table}, polyhedron, data.table containing at least 
#' two ptdf columns :
#' \itemize{
#'  \item ptdfAT : autrichian vertices
#'  \item ptdfBE : belgium vertices
#'  \item ptdfDE : german vertices
#'  \item ptdfFR : french vertices
#' }
#' If B is NULL, the heatmap returns the absolute volume of A in the different directions.
#' @param nbPoints \code{numeric}, number of points generated for volume comparison
#' @param seed \code{numeric} fixed random seed, used for the weighted draw of the
#' points for volume assessment. By default, the value is 123456
#' @param assessment_range The range in which the points for the volume assessment should be drawn
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' polyhedra <- readRDS(system.file("testdata/polyhedra.rds", package = "fbAntares"))
#' A <- polyhedra[Date == "2019-02-14"]
#' B <- polyhedra[Date == "2019-02-15"]
#' nbPoints <- 50000
#' 
#'  drawVolumeHeatmapTwoDimensions(A = A, B = B, nbPoints = nbPoints)
#' }
#' 
#' @import data.table
#' @importFrom plotly plot_ly
#' @import stringr
#' 
#' @export

drawVolumeHeatmapTwoDimensions <- function(A, B = NULL, nbPoints = 50000, seed = 123456, assessment_range = c(-15000, 15000)){
  
  col_ptdf <-  .crtlPtdf(A, B)
  
  col_ptdf_signed <- as.vector(outer(col_ptdf[-length(col_ptdf)], c("positive", "negative"), paste, sep = "."))
  
  heatmap <- sapply(col_ptdf_signed, function(X){
    
    combns <- sapply(col_ptdf_signed, function(elt){paste0(X, "-", elt)})
    combns_split <- strsplit(combns, "-")
    combns_split <- lapply(combns_split, str_remove, "ptdf")
    
    result <- sapply(combns_split, function(Y){
      
      Y_split <- strsplit(Y, "[.]")
      countries <- sapply(Y_split, function(vec){vec[1]})
      if(countries[1] == countries[2]) return(NA)
      dirs <- sapply(Y_split, function(vec){vec[2]})
      direction <- data.table(country = countries, direction = dirs)
      
      volume_A <- evalDomainVolume(A, nbPoints, seed, assessment_range, direction = direction)
      
      if(!is.null(B)){
        volume_B <- evalDomainVolume(B, nbPoints, seed, assessment_range, direction = direction)
        
        diff_volume <- (volume_B - volume_A) / volume_A * 100
      } else {
        volume_A
      }
      
    })
    names(result) <- sapply(combns_split, function(vec){vec[2]})
    unlist(result)
    
  })
  
  colnames(heatmap) <- str_remove(colnames(heatmap), "ptdf")
  
  colorlist <- .returnColors(c(heatmap))
  colorlist[, vals := scales::rescale(vals)]
  
  fig <- plot_ly(x = colnames(heatmap), y = rownames(heatmap), z = heatmap, colorscale = colorlist, type = "heatmap")
  fig
  
}

#' @title Draw a volume heatmap comparing the volumes of two polyhedrons making combinations
#' over the range of three possible net positions
#' 
#' @description This function returns a 3D heatmap comparing the volumes of two polyhedrons
#' in different directions corresponding to all the combinations of the positive/negative
#' net positions of each of the three countries provided in the \code{ptdf_list} argument.
#' 
#' @param A \code{data.table}, polyhedron, data.table containing at least 
#' two ptdf columns :
#' \itemize{
#'  \item ptdfAT : autrichian vertices
#'  \item ptdfBE : belgium vertices
#'  \item ptdfDE : german vertices
#'  \item ptdfFR : french vertices
#' }
#' @param B \code{data.table}, polyhedron, data.table containing at least 
#' two ptdf columns :
#' \itemize{
#'  \item ptdfAT : autrichian vertices
#'  \item ptdfBE : belgium vertices
#'  \item ptdfDE : german vertices
#'  \item ptdfFR : french vertices
#' }
#' If B is NULL, the heatmap returns the absolute volume of A in the different directions.
#' @param nbPoints \code{numeric}, number of points generated for volume comparison
#' @param seed \code{numeric} fixed random seed, used for the weighted draw of the
#' points for volume assessment. By default, the value is 123456
#' @param assessment_range The range in which the points for the volume assessment should be drawn
#' @param ptdf_list The list of ptdfs over which the heatmap must be drawn. They are names of columns
#' of A and B.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' polyhedra <- readRDS(system.file("testdata/polyhedra.rds", package = "fbAntares"))
#' A <- polyhedra[Date == "2019-02-14"]
#' B <- polyhedra[Date == "2019-02-15"]
#' nbPoints <- 50000
#' 
#'  drawVolumeHeatmapThreeDimensions(A = A, B = B, nbPoints = nbPoints, ptdf_list = c("ptdfFR", "ptdfBE", "ptdfDE"))
#' }
#' 
#' @import data.table
#' @import stringr
#' 
#' @export

drawVolumeHeatmapThreeDimensions <- function(A, B = NULL, nbPoints = 50000, seed = 123456, assessment_range = c(-15000, 15000), ptdf_list){
  
  col_ptdf <-  .crtlPtdf(A, B)
  
  if(length(setdiff(ptdf_list, col_ptdf) > 0)){
    stop(paste("All the PTDFs in the argument ptdf_list must be columns of your domains A and B"))
  }
  
  combn <- .getDrawingDirection3DHeatmap(ptdf_list)
  countries <- sapply(ptdf_list, str_remove, "ptdf")
  
  combn[, compared_volume := apply(.SD, MARGIN = 1, FUN = function(X){
    direction <- data.table(country = countries, direction = X)
    
    volume_A <- evalDomainVolume(A, nbPoints, seed, assessment_range, direction = direction)
    
    if(!is.null(B)){
      volume_B <- evalDomainVolume(B, nbPoints, seed, assessment_range, direction = direction)
      
      diff_volume <- (volume_B - volume_A) / volume_A * 100
      diff_volume
    } else {
      volume_A
    }
  })]
  
  fig <- .draw3DHeatmap(combn$compared_volume, countries)
  fig
  
}

# Return the colors for the heatmap of a series of values positive (colored in green) of negative (colored in red)
#' @import data.table
#' @import scales
.returnColors <- function(vals) {
  
  vals <- as.data.table(vals)
  vals <- unique(vals)
  
  vals_positive <- vals[vals >= 0]
  if(length(vals_positive > 0)){
    vals_positive[, color := sapply(.SD, function(X){
      vals_uniform_positive <- scales::rescale(X, from = c(0, max(X)))
      colors_positive <- scales::col_numeric("Greens", domain = c(0, 1))(vals_uniform_positive)
      colors_positive
    })]
  } else {
    vals_positive <- data.table()
  }
  
  vals_negative <- vals[vals < 0]
  if(length(vals_negative >0)){
    vals_negative[, color := sapply(.SD, function(X){
      vals_uniform_negative <- scales::rescale(X, from = c(0, min(X)))
      colors_negative <- scales::col_numeric("Reds", domain = c(0, 1))(vals_uniform_negative)
      colors_negative
    })]
  } else {
    vals_negative <- data.table()
  }
  
  vals_tot <- unique(rbind(vals_positive, vals_negative))
  vals_merged <- unique(merge(vals, vals_tot), all.x = T)
  
  vals_merged
}

#Returns the drawing directions of the cubes for the 3D heatmap
#' @import data.table
.getDrawingDirection3DHeatmap <- function(ptdf_list){
  
  dt <- as.data.table(expand.grid(c("positive", "negative"), c("positive", "negative"), c("positive", "negative")))
  colnames(dt) <- ptdf_list
  dt
  
}

#Draws the 3D heatmap from the list of values provided
#' @importFrom plotly plot_ly add_trace layout
.draw3DHeatmap <- function(vals, countries){
  
  i <- c(7, 0, 0, 0, 4, 4, 6, 6, 4, 0, 3, 2)
  j <- c(3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3)
  k <- c(0, 7, 2, 3, 6, 7, 1, 1, 5, 5, 7, 6)
  
  x <- c(0, 0, 1, 1, 0, 0, 1, 1)
  y <- c(0, 1, 1, 0, 0, 1, 1, 0)
  z <- c(0, 0, 0, 0, 1, 1, 1, 1)
  
  combinations <- t(expand.grid(c(0, -1), c(0, -1), c(0, -1)))
  
  colors_raw <- .returnColors(vals)
  colors <- data.table(vals_ordered = vals)
  colors[, color := sapply(vals_ordered, function(X){
    colors_raw[vals == X, color]
  })]
  
  
  vals[vals > 1000] <- unlist(sapply(vals[vals>1000], formatC, format = "e", digits = 2))
  
  fig <- plot_ly() %>% add_trace(type = 'mesh3d',
                                 x = x,
                                 y = y,
                                 z = z,
                                 i = i,
                                 j = j,
                                 k = k,
                                 facecolor = rep(colors[1,]$color, 12),
                                 hoverinfo = "text",
                                 hovertext = vals[1])
  
  for(index in 1:7){
    combination <- combinations[, index + 1]
    fig <- fig %>% add_trace(type = 'mesh3d',
                             x = x + combination[1],
                             y = y + combination[2],
                             z = z + combination[3],
                             i = i,
                             j = j,
                             k = k,
                             facecolor = rep(colors[index + 1,]$color, 12),
                             hoverinfo = "text",
                             hovertext = vals[index + 1])
  }
  
  
  for(index in 1:8){
    combination <- combinations[, index]
    fig <- fig %>% add_trace(x = x + combination[1],
                             y = y + combination[2],
                             z = z + combination[3],
                             type = "scatter3d",
                             mode = "markers",
                             name = vals[index],
                             marker = list(color = colors[index,]$color, size = 5),
                             hoverinfo = "text",
                             hovertext = vals[index])
  }
  
  names_axes <- sapply(countries, function(X){paste0("NP ", X)})
  
  fig <- fig %>% layout(scene = list(xaxis = list(title = names_axes[1]),
                                     yaxis = list(title = names_axes[2]),
                                     zaxis = list(title = names_axes[3])),
                        legend=list(title=list(text='<b> Volume </b>')))
  
  fig
  
}
