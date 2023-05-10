#' Theme Convenience Functions
#' 
#' @description
#' \code{ggtern} has made available a number of convenience functions for rapid tweaking of the various theme elements, 
#' for a full list of the available theme elements which can be manually modified, see \link[=theme_elements]{HERE}.
#' 
#' @section Convenience Functions:
#' Some of the Convenience functions that ship with \code{ggtern}, to assist in the rapid modification of
#' key theme elements:
#' \itemize{
#'   \item \code{\link[=theme_showtitles]{Show/Hide Axis Titles}}
#'   \item \code{\link[=theme_showarrows]{Show/Hide Arrows}} 
#'   \item \code{\link[=theme_showgrid]{Show/Hide Grids}}
#'   \item \code{\link[=theme_showlabels]{Show/Hide Axis Ticklabels}}
#'   \item \code{\link[=theme_showprimary]{Show/Hide Primary/Secondary Ticks}}
#'   \item \code{\link[=theme_ticksinside]{Ticks Inside or Outside of the Main Plot Area}}
#'   \item \code{\link[=theme_arrowcustomlength]{Set Length of arrows}} 
#'   \item \code{\link[=theme_clockwise]{Clockwise/Anticlockwise Axis Precession}} 
#'   \item \code{\link[=theme_rotate]{Rotate the plot by X degrees or radians}}
#'   \item \code{\link[=theme_mesh]{Create a mesh of 'n' Major/Minor gridlines}}
#'   \item \code{\link[=theme_latex]{Enable/Disable parsing of labels according to latex markup}}
#'   \item \code{\link[=theme_showmask]{Turn off the clipping mask}}
#'   \item \code{\link[=custom_percent]{Atomic or Weight Percent Arrow Label Suffix.}}
#' }
#' 
#' @section Manual Modification:
#' For manual modification on a per-element basis:
#' \itemize{
#'   \item \code{\link[=theme_elements]{Ternary Theme Elements}} 
#' }
#' 
#' @section Default Themes:
#' Default (complete) themes which ship with \code{ggtern}:
#' \itemize{
#'   \item \code{\link[=theme_complete]{Complete Themes}} 
#' }
#' 
#' @aliases convenience_functions theme_convenience
#' @name theme_convenience_functions
#' @rdname theme_convenience_functions
#' @examples
#' 
#' #Load data and create the base plot.
#' plot <- ggtern() + theme_bw() + 
#'  theme(tern.axis.ticks.length.major=unit(3.0,'mm'),
#'        tern.axis.ticks.length.minor=unit(1.5,'mm'))
#' plot
#' 
#' #Show Arrows
#' last_plot() + theme_showarrows()
#' 
#' #Major/Minor Grids?
#' last_plot() + theme_nogrid_minor()
#' last_plot() + theme_nogrid_major()
#' last_plot() + theme_showgrid()
#' 
#' #Clockwise/Anticlockwise Precession
#' last_plot() + theme_clockwise()
#' 
#' #Ticks Inside or Outside
#' last_plot() + theme_ticksinside()
#' 
#' #Show/Hide BOTH Primary and Secondary Ticks
#' last_plot() + theme_showticks()
#' last_plot() + theme_hideticks()
#' 
#' #Show/Hide EITHER Primary OR Secondary Ticks.
#' last_plot() + theme_showprimary() + theme_hidesecondary()
#' last_plot() + theme_hideprimary() + theme_showsecondary()
#' 
#' #Atomic / Weight Percent
#' last_plot() + theme_showarrows() + atomic_percent() #+weight_percent()
#' last_plot() + theme_showarrows() + custom_percent("Atomic Percent")
#' 
#' #Rotation
#' last_plot() + theme_rotate(60)
NULL
