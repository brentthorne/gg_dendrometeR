#' Create's a ggplot for dendrometer cell phase data.
#'
#' Can be displayed as a single plot or as a facet wrap for each unique dendrometer.
#'
#' @usage gg_cellphase(gg_cellphase_data, start.day, end.day, date_breaks, date_labels, type = "all")
#'
#' @param gg_cellphase_data a \code{data.frame} with either gap-free or gap-filled dendrometer phase series as produced by \link{gg_cellphase_data}.
#' @param start.day a \code{timestamp} specifying the begining of dendrometer data to be used in the final plot timestamp format is (\code{\%Y-\%m-\%d \%H:\%M:\%S} format).
#' @param end.day a \code{timestamp} specifying the end of dendrometer data to be used in the final plot timestamp format is (\code{\%Y-\%m-\%d \%H:\%M:\%S} format).
#' @param date.breaks a \code{string} giving the distance between breaks such as: "2 weeks", or "10 years".
#' @param date.labels a \code{string} giving the formatting specification for the labels. Codes are defined in \link[base]{strftime}. If both labels and date_labels are specified, date_labels wins.
#' @param type a \code{string} specifying either 1) the default \code{type = "all"}; a single plot with all dendrometer data on it, or 2) \code{type = "facet"}; if the plot should be displayed based on the number of dendrometers used within the users origional data frame.
#'
#'@author Brent Thorne
#'
#' @return The function returns a \code{geom_point} plot or can be stored as an object.
#'
#' @import dendrometeR
#' @import tidyverse
#'
#' @examples
#' \donttest{
#' #plot all on one
#' library(dendrometeR)
#' data(dmCD)
#' dm.phase <- phase_def(dmCD)
#' dm.cellphase <- gg_cellphase_data(dmCD)
#' gg_cellphase(gg_cellphase_data = dm.cellphase)
#'
#' #plot facet for each dendrometer
#' library(dendrometeR)
#' data(dmED)
#' dmED <- fill_gaps(dmED)
#' dm.phase <- phase_def(dmED)
#' dm.cellphase <- gg_cellphase_data(dmED)
#' gg_cellphase(gg_cellphase_data = dm.cellphase, type = "facet")
#'}
#'
#' @export gg_cellphase
#'

gg_cellphase <- function(gg_cellphase_data,
                         start.day=gg_cellphase_data[1,"timestamp"],
                         end.day=gg_cellphase_data[nrow(gg_cellphase_data),"timestamp"],
                         date.breaks="1 months",
                         date.labels="%m",
                         type = "all"){



  if (type == "all"){

    ggcellphase <- ggplot(data = gg_cellphase_data, aes(x=timestamp, y= amplitude, colour = factor(Phase)))+
      theme(axis.text.x = element_text(angle = 65, hjust = 1))+
      theme_bw()+
      scale_x_datetime(date_breaks = date.breaks,
                       date_labels = date.labels,
                       limits = as.POSIXct(c(start.day,end.day),
                                           format = "%Y-%m-%d %H:%M:%S"))+
      geom_point()+
      scale_color_manual(values = c("darkslategray3","darkslategray4","darkslategray"), aes("Phase"))
    print(ggcellphase)

    }else{ if (type == "facet"){

            ggcellphase <- ggplot(data = gg_cellphase_data, aes(x=timestamp, y= amplitude, colour = factor(Phase)))+
              theme(axis.text.x = element_text(angle = 65, hjust = 1))+
              theme_bw()+
              scale_x_datetime(date_breaks = date.breaks,
                               date_labels = date.labels,
                               limits = as.POSIXct(c(start.day,end.day),
                                                   format = "%Y-%m-%d %H:%M:%S"))+
              geom_point(aes(colour=factor(Phase)))+
              scale_color_manual(values = c("darkslategray3","darkslategray4","darkslategray"), aes("Phase"))+
              facet_wrap(~dmID)
            print(ggcellphase)

    }else{
             warning("Please select 'all' or 'facet' for argument 'type='.")
           }
        }
}
