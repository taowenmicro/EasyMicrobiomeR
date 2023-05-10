#' Modified Aesthetic Mappings
#' 
#' @details 
#' An extension to the base aes functin from ggplot2, this is modified to handle a default z mapping 
#' for application in ternary phase diagrams. Does not alter the standard behaviour. 
#' 
#' @param x x value
#' @param y y value
#' @param z z value
#' @param ... other arguments as per \code{\link[ggplot2]{aes}}
#' @seealso Parent \code{\link[ggplot2]{aes}} function.
#' @rdname aes
#' @export
aes <- function(x,y,z,...) {
  aes <- structure(as.list(match.call()[-1]), class = "uneval")
  rename_aes(aes)
}

# Rename American or old-style aesthetics name
rename_aes <- function(x) {
  aa = c(getFromNamespace('.all_aesthetics','ggplot2'),"T","L","R","zend")
  # Convert prefixes to full names
  full <- match(names(x),aa)
  names(x)[!is.na(full)] <- aa[full[!is.na(full)]]
  plyr::rename(x, find_global_tern(".base_to_ggplot"), warn_missing = FALSE)
}

# Look up the scale that should be used for a given aesthetic -- ternary version
aes_to_scale_tern = function (var){
  var = ggint$aes_to_scale(var)
  var[var %in% c("z", "zmin", "zmax", "zend", "zintercept")] <- "z"
  var
}

# Figure out if an aesthetic is a position aesthetic or not
is_position_aes <- function(vars) {
  aes_to_scale_tern(vars) %in% c("x", "y", "z")
}

