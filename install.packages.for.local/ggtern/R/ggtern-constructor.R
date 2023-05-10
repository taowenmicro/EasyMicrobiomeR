#' ggtern Constructor
#' 
#' Plots in \code{ggtern} are instigated via the default constructor: \code{ggtern(...)}, which is essentially a convenience wrapper for the following: 
#' \code{ggplot{...} + coord_tern()}, indeed, if one wishes to use \code{ggplot{...} + coord_tern()} then this is quite satisfactory.
#' @inheritParams ggplot2::ggplot
#' @param ... additional arguments passed through to \code{\link{ggplot}}
#' @aliases constructor
#' @return \code{ggtern(...)} returns an object of class \code{ggplot}.
#' @seealso For an introduction to the \code{ggtern} package, (including many examples), click \link[=ggtern-package]{HERE}.
#' @author Nicholas Hamilton
#' @examples 
#' ggtern(data=data.frame(x=1,y=1,z=1),aes(x,y,z)) + geom_point()
#' @export
ggtern <- function(data=NULL,mapping=aes(),...,environment=parent.frame()){
  ## Suppress the warning notice of new coordinate system
  suppressMessages({ 
    ggplot(data = data, mapping = mapping, environment = environment, ... ) + coord_tern()  
  })
}