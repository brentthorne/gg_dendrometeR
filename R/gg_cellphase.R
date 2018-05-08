#' Create's a ggplot friendly data table for the dm.phase product.
#'
#' Transform the phase data into a single ggplot2 friendly data frame.
#'
#' @usage gg_cellphase(dm.gpf, resolution = dendro.resolution(dm.gpf),shapeSensitivity = 0.6, minmaxDist = 0.2,minmaxSD = 2, radialIncrease = "max", na.omit = TRUE)
#'
#' @param dm.gpf a \code{data.frame} with either gap-free or gap-filled dendrometer series as produced by \code{\link[dendrometeR]{fill_gaps}}.
#' @param resolution a \code{numeric} specifying the resolution of the dendrometer data in seconds. Defaults to the resolution of \code{dm.gpf} as calculated using \code{\link[dendrometeR]{dendro.resolution}}.
#' @param shapeSensitivity a \code{numeric} specifying a time window, defined as proportion of a single day. Within this time window possible extrema points (i.e. minimum and maximum) in dendrometer measurements are searched for. Defaults to 0.6 (60\% of a day). See details for further explanation.
#' @param minmaxDist a \code{numeric} specifying the minimum temporal distance between consecutive minimum and maximum points (i.e. in the x direction). Defaults to 0.2 (20\% of a day).
#' @param minmaxSD a \code{numeric} specifying the minimum difference between consecutive minimum and maximum points expressed as a number of standard deviations (i.e. in the y direction). Defaults to 2.
#' @param radialIncrease a \code{character} string of \code{"max", "min", "mid"}, specifying when the stem-radius increment phase should start, with \code{"max"} as the most, and \code{"min"} as the least conservative approach; \code{"mid"} is in between. See details for further explanation.
#' @param na.omit a \code{logical opperator} statement of either \code{TRUE} or \code{FALSE}, specifying weather or not to remove \code{NA} values from final dataset. The default is set to \code{na.omit = TRUE}.
#'
#' @author Brent Thorne
#'
#' @return The function returns a \code{data.frame} containing the following summary statistics:
#' \item{dmID}{dendrometer ID.}
#' \item{Phase}{cyclic phase (1: contraction, 2: expansion, 3: stem-radius increment, 4: full cycle).}
#' \item{timestamp}{timestamp indicating the date and time of phase stage.}
#'
#' @import dendrometeR
#' @import tidyverse
#'
#' @examples
#' \donttest{
#' library(dendrometeR)
#' data(dmCD)
#' dm.phase <- phase_def(dmCD)
#' dm.cellphase <- gg_cellphase(dmCD)
#'}
#'
#' @export gg_cellphase
#'


gg_cellphase <- function(dm.gpf, resolution = dendro.resolution(dm.gpf),
                         shapeSensitivity = 0.6, minmaxDist = 0.2,
                         minmaxSD = 2, radialIncrease = "max", na.omit = TRUE){

  #calculate dm.phase
  dm.phase <- phase_def(dm.gpf = dm.gpf,resolution = resolution,
                        shapeSensitivity = shapeSensitivity, minmaxDist = minmaxDist,
                        radialIncrease = radialIncrease)

  #Create empty list for data to be re-organized into
  gg.data <- c()

  #Run a loop to create appropriate data formatting for ggplot2
  i=1
  for (i in 1:as.numeric(ncol(dm.gpf))){

    tmp.data           <- data.frame(dm.phase[,i])
    tmp.data$dmID      <- as.character(colnames(dm.phase)[i])
    tmp.data$timestamp <- as.POSIXct(rownames(dm.phase), format = "%Y-%m-%d %H:%M:%S")
    gg.data            <- rbind(gg.data, tmp.data)

  }
  #Final changes to data structure for use in ggplot2
  if (na.omit == TRUE){
  #if na.omit is == TRUE
  gg.data              <- na.omit(gg.data)
  gg.data$dmID         <- as.factor(gg.data$dmID)
  colnames(gg.data)[1] <- "Phase"
  }else{
    #if na.omit == FALSE
    gg.data$dmID         <- as.factor(gg.data$dmID)
    colnames(gg.data)[1] <- "Phase"
    }

  gg.data <- select(gg.data, dmID, Phase, timestamp)

  return(gg.data)

}
