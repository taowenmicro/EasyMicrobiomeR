#' Depreciated Functions
#' 
#' The following is a list of functions which were once used in previous versions of ggtern, however,
#' have now been depreciated
#' @name zzz-depreciated
#' @rdname zzz-depreciated
#' @keywords depreciated
#' @author Nicholas Hamilton
#' 
#' @param src character name of current procedure 
#' @param df a data frame
#' @param coord a ternary coordinate system
#' @param op operation method to clip, intersection, union, minus or xor
#' @param plyon items in the data frame to pass to ddply argument
#' @param label a character ('axis','ticks' or 'labels') or numeric (rounded to 0, 1 or 2) value to determine the relative location (labels is default)
#' if a character is provided, and it is not one of the above, an error will be thrown.
#' @param showarrows logical whether to show the axis directional arrows DEPRECIATED
#' @param padding the padding around the plot area to make provision for axis labels, ticks and arrows, relative to the cartesian plane. DEPRECIATED
#' @param arrowsep the distance between ternary axis and ternary arrows DEPRECIATED
#' @param arrowstart the proportion along the ternary axis to start the directional arrow DEPRECIATED
#' @param arrowfinish the proportion along the ternary axis to stop the directional arrow DEPRECIATED
#' @param vshift shift the plot area vertically DEPRECIATED
#' @param hshift shift the plot area horizontally DEPRECIATED
#' @param ticklength.major the length of the major ternary ticks as an euclidean distance 
#' relative to the x and y limits of the cartesian plot area. DEPRECIATED
#' @param ticklength.minor the length of the minor ternary ticks as an euclidean distance 
#' relative to the x and y limits of the cartesian plot area. DEPRECIATED
#' @param ... additional arguments, multiple plot objects
#' @param plotlist alternative to the ... argument, provide a list of ggplot or grob objects, objects which do not inherit 
#' the ggplot or grob classes will be stripped.
#' @param cols number of columns if the layout parameter is not provided.
#' @param layout override number of cols, and provide a matrix specifying the layout
#' @param x vector of numeric \code{x} values
#' @param y vector of numeric \code{y} values
#' @param close logical value (default \code{FALSE}), as to whether the set should be closed by adding (duplicating) 
#' the first row (after ordering) to the end of the set.
NULL


#' Stop Procedure
#' 
#' \strong{DEPRECIATED:} \code{tern_stop(\dots)} Internal Function, checks if the most recent coordinate system is ternary, and, if not, 
#' stops the current procedure, with a common message format
#' @rdname zzz-depreciated
tern_stop <- function(src="target"){
  tern_dep("1.0.6.1",'tern_stop no longer in use')
}

#' Clip Polygons
#' 
#' \strong{DEPRECIATED:} \code{clipPolygons(\dots)} Using the using the PolyClip Package, This clips input polygons for 
#' use in the density and contour geometries.
#' @aliases polyclip
#' @keywords polygon clipping
#' @rdname zzz-depreciated
clipPolygons <- function(df,coord,plyon=c('level','piece','group'),op="intersection"){
  tern_dep("1.0.6.1",'clipPolygons no longer in use')
}

#' Set The Ternary Arrow Baseline
#' 
#' \strong{DEPRECIATED:} \code{theme_arrowbaseline(\dots)} The ternary arrows can have an offset unit value (see \code{tern.axis.arrow.sep}), 
#' however, it is convenient to set this relative to either the axis, ticks or axis ticklabels (since the latter
#'  two can be hidden / removed.). This function permits this to be set
#' @rdname zzz-depreciated
#' @export
theme_arrowbaseline <- function(label='labels'){
  tern_dep('1.0.6.1','theme_arrowbaseline() is no longer used, consider the tern.axis.arrow.sep theme element')
}


#' Theme element: ternary structure
#' 
#' \strong{DEPRECIATED:} \code{element_ternary(\dots)} Replaced by individual theme elements:
#' \enumerate{
#'   \item \code{tern.axis.arrow.show}
#'   \item \code{tern.axis.padding}
#'   \item \code{tern.axis.arrow.sep}
#'   \item \code{tern.axis.arrow.start}
#'   \item \code{tern.axis.arrow.finish}
#'   \item \code{tern.axis.vshift}
#'   \item \code{tern.axis.hshift}
#'   \item \code{tern.axis.ticks.length.major}
#'   \item \code{tern.axis.ticks.length.minor}
#' }
#' 
#' Used to define the layout of some of the ggtern plot features which are unique to the ternary diagrams , and hence, this package.
#' 
#' @rdname zzz-depreciated
#' @export
element_ternary <- function(showarrows,
                            padding,
                            arrowsep,
                            arrowstart,
                            arrowfinish,
                            vshift,
                            hshift,
                            ticklength.major,
                            ticklength.minor){
  tern_dep('1.0.1.3','element_ternary is no longer in use')
  if(!missing(ticklength.major))
    tern_dep("1.0.1.3","ticklength.major has been replaced by element 'tern.axis.ticks.length.major'")
  if(!missing(ticklength.minor))
    tern_dep("1.0.1.3","ticklength.minor has been replaced by element 'tern.axis.ticks.length.minor'")
  if(!missing(showarrows))
    tern_dep("1.0.1.3","showarrows has been replaced by element 'tern.axis.arrow.show'")
  if(!missing(padding))
    tern_dep("1.0.1.3","padding has been replaced by element 'tern.axis.arrow.show'")
  if(!missing(arrowsep))
    tern_dep("1.0.1.3","arrowsep has been replaced by element 'tern.axis.arrow.sep'")
  if(!missing(vshift))
    tern_dep("1.0.1.3","vshift has been replaced by element 'tern.axis.vshift'")
  if(!missing(hshift))
    tern_dep("1.0.1.3","hshift has been replaced by element 'tern.axis.hshift'")
  if(!missing(arrowstart))
    tern_dep("1.0.1.3","arrowstart has been replaced by element 'tern.axis.arrow.start'")
  if(!missing(arrowfinish))
    tern_dep("1.0.1.3","hshift has been replaced by element 'tern.axis.arrow.finish'")
  
  #Return the object.
  structure(
    list(),
    class = c("element_ternary")
  )
}

#' Arrange Multiple Plot Objects
#'
#' \strong{DEPRECIATED:} \code{ggtern.multi} is a function which permits the arrangement of muliple \code{ggtern} or \code{ggplot2} objects,
#' plots can be provided to the elipsis argument, or, as a list and at the simplest case, the number of columns can be
#' specified. For more advanced usage, consider the layout argument.
#'
#' By default, 1 column is specified, which means that the plots will be stacked on top of each other in a single column,
#' however, if say 4 plots are provided to the ellipsis or \code{plotlist}, with \code{cols} equal to 2, 
#' then this will produce a 2 x 2 arrangement.
#' 
#' In regards to the \code{layout} argument (which overrides the \code{cols} argument), if it is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' then plot number 1 will go in the upper left, 2 will go in the upper right, and 3 will go all the way across the 
#' bottom - see the last example below.
#' @source http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#' @aliases multi multiplot
#' @rdname zzz-depreciated
#' @export
ggtern.multi <- function(..., plotlist = NULL, cols = 1, layout = NULL) {
  tern_dep("1.0.6.1","No longer used, use grid.arrange instead which now works flawlessly")
}

#' Put Points in Sequence
#'
#' \strong{DEPRECIATED:} The \code{point.in.sequence} function takes numeric input vectors \code{x} and \code{y} or a 
#' \code{\link{data.frame}} object, and orders the values in such way that they are correctly sequenced by the angle subtended between each point,
#' and, the centroid of the total set. If the data is provided in the format of a \code{data.frame}, then it must 
#' containing columns named \code{x} and \code{y}, else an error will be thrown.
#' 
#' The arguments \code{x} and \code{y} represent cartesian coordinates. This is useful if a path is sought that 
#' passes through each point in the ordered set, however, no two lines in the total path cross over each other. 
#' Uses the \code{\link{atan2}} function to determine the angle (theta) between each point (x,y) and the centroid 
#' of the data, it then orders based on increasing values of theta. 
#' 
#' @return \code{data.frame} object containing the re-ordered input set.
#' @rdname zzz-depreciated
#' @export  
point.in.sequence <- function(x,y,...,df=data.frame(x=x,y=y),close=FALSE){
  tern_dep('1.0.6.1','no longer supported or required')
}
