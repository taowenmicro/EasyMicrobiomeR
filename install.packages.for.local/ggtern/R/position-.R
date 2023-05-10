#' Convenience function to transform all position variables in a ternary plot
#'
#' @param trans_x,trans_y,trans_z Transformation functions for x, y and z aesthetics.
#'   (will transform x, xmin, xmax, xend etc)
#' @param ... Additional arguments passed to \code{trans_x} and \code{trans_y}.
#' @keywords internal
#' @author Nicholas Hamilton
#' @rdname transform_position_tern
#' @export
transform_position_tern <- function(df, trans_x = NULL, trans_y = NULL, trans_z = NULL, ...) {
  scales <- aes_to_scale_tern(names(df))
  
  if (!is.null(trans_x)) {
    df[scales == "x"] <- lapply(df[scales == "x"], trans_x, ...)
  }
  if (!is.null(trans_y)) {
    df[scales == "y"] <- lapply(df[scales == "y"], trans_y, ...)
  }
  if (!is.null(trans_z)) {
    df[scales == "z"] <- lapply(df[scales == "z"], trans_z, ...)
  }
  df
}