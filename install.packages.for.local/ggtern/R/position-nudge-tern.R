#' Nudge Ternary Points.
#'
#' This is useful if you want to nudge labels a little ways from their points, input data will normalised to sum to unity before applying the particular
#' nudge, so the nudge variables should be as a fraction ie (0,1)
#'
#' @family position adjustments
#' @param x,y,z Amount of compositions to nudge
#' @author Nicholas Hamilton
#' @rdname position_nudge_tern
#' @export
position_nudge_tern <- function(x = 0, y = 0, z = 0) {
  ggproto(NULL, PositionNudgeTern, x = x, y = y, z = z )
}

#' @rdname position_nudge_tern
#' @format NULL
#' @usage NULL
#' @export
PositionNudgeTern <- ggproto("PositionNudgeTern", ggplot2::Position,
                         x = NULL, y = NULL, z = NULL,
                         required_aes = c("x","y","z"),
                         setup_params = function(self, data) {
                           list(x = self$x %||% 0, 
                                y = self$y %||% 0, 
                                z = self$z %||% 0)
                         },
                         compute_layer = function(self,data, params, panel) {
                           data[,self$required_aes] = as.data.frame(acomp(data[,self$required_aes]))
                           data = transform_position_tern(data, 
                                                   function(x) x + params$x, 
                                                   function(y) y + params$y,
                                                   function(z) z + params$z)
                           data
                         }
)
