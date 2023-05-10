#' Rotate Ternary Diagram
#' 
#' Convenience function to rotate the diagram by an angle in degrees or radians.
#' @param degrees,radians specify the angle to rotate the plot by in either degrees or radians. 
#' If both \code{degrees} and \code{radians} are specified, then precedence is given to 
#' the \code{radians} argument. If no value is specified, the plot will rotate by 60 degrees
#' @examples 
#' x = ggtern(data.frame(x=1,y=1,z=1),aes(x,y,z))
#' for(a in seq(0,60,by=15))
#' print(x + theme_rotate(a))
#' @author Nicholas Hamilton
#' @rdname theme_rotate
#' @export
theme_rotate = function(degrees=60,radians=degrees*pi/180){ theme(tern.panel.rotate = radians*180/pi) }
