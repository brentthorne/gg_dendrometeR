#' Cycle stats for all sensors
#'
#' Calculate cycleStats using the \link[dendrometeR]{cycle_stats} and produce single df for all sensor in you data. The dendrometeR package
#' does not allow for this currently
#'
#' @usage all_cycleStats(dm.gpf, dm.phase, smooth.param = 1)
#'
#' @param dm.gpf a \code{data.frame} with either gap-free or gap-filled dendrometer series as produced by \code{\link[dendrometeR]{fill_gaps}}.
#' @param dm.phase a \code{data.frame} with numbers indicating the different stem-cyclic phases. Output of \code{\link[dendrometeR]{phase_def}}.
#' @param smooth.param a \code{numeric} specifying the degree of smoothing. Defaults to 1 (no smoothing).
#'
#' @author Brent Thorne
#'
#' @return The function returns a \code{data.frame} containing the following summary statistics:
#' \item{dmID}{dendrometer ID.}
#' \item{cycle}{cycle number.}
#' \item{phase}{cyclic phase (1: contraction, 2: expansion, 3: stem-radius increment, 4: full cycle).}
#' \item{begin}{timestamp indicating the beginning of each phase.}
#' \item{end}{timestamp indicating the end of each phase.}
#' \item{duration_h}{phase duration in hours.}
#' \item{duration_m}{phase duration in minutes.}
#' \item{magnitude}{magnitude of stem-size changes in each phase.}
#' \item{min}{minimum stem size within each phase.}
#' \item{max}{maximum stem size within each phase.}
#'
#' @import dendrometeR
#'
#' @examples
#' \donttest{
#' library(dendrometeR)
#' data(dmCD)
#' dm.phase <- phase_def(dmCD)
#' cycleStats.all <- all_cycleStats(dmCD,dm.phase)
#'}
#'
#' @export all_cycleStats
#'


all_cycleStats <- function(dm.gpf, dm.phase, smooth.param = 1){
  cycleStats.all <- c()
  i=1
  for (i in 1:as.numeric(ncol(dm.gpf))){
    cycleStats.tmp <- cycle_stats(dm.gpf = dm.gpf, dm.phase = dm.phase, sensor = as.numeric(i), smooth.param = smooth.param)
    cycleStats.table <- cycleStats.tmp[[1]]
    cycleStats.all <- rbind(cycleStats.all, cycleStats.table)
  }
  return(cycleStats.all)
}
