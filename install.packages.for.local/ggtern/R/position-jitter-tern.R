#' Jitter Ternary Points
#' 
#' Jitter ternary points to avoid overplotting.
#' @family position adjustments
#' @param x,y,z amount of positional jitter
#' @author Nicholas Hamilton
#' @rdname position_jitter_tern
#' @export
position_jitter_tern <- function(x = NULL, y = NULL, z=NULL) {
  ggproto(NULL, PositionJitterTern,
          x = x,
          y = y,
          z = z
  )
}

#' @rdname position_jitter_tern
#' @format NULL
#' @usage NULL
#' @export
PositionJitterTern <- ggproto("PositionJitterTern", ggplot2::Position,
  required_aes = c("x", "y","z"),
  setup_data   = function(self,data,params){
    data[,self$required_aes] = as.data.frame(acomp(data[,self$required_aes]))
    data
  },
  setup_params = function(self, data) {
    list(
      x  = self$x %||% resolution(data$x, zero = FALSE) * 0.4,
      y  = self$y %||% resolution(data$y, zero = FALSE) * 0.4,
      z  = self$z %||% resolution(data$z, zero = FALSE) * 0.4
    )
  },
  
  compute_layer = function(data, params, panel) {
    trans_x <- if (params$x > 0) function(x) jitter(x, amount = params$x)
    trans_y <- if (params$y > 0) function(y) jitter(y, amount = params$y)
    trans_z <- if (params$z > 0) function(z) jitter(z, amount = params$z)
    transform_position_tern(data, trans_x, trans_y, trans_z)
  }
)
