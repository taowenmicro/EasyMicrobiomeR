#' Ternary Position Scales
#' 
#' Define the ternary continuous position scales (T, L & R).
#' @inheritParams ggplot2::scale_x_continuous
#' @param ... not used
#' @author Nicholas Hamilton
#' @rdname scale_X_continuous
#' @name scale_X_continuous
#' @export
scale_T_continuous <- function(name         = waiver(), 
                               limits       = NULL,
                               breaks       = waiver(),
                               minor_breaks = waiver(),
                               labels       = waiver(),...) {
  sc <- continuous_scale(
    c("T"),
    "tern_T", identity, name = name, breaks = breaks,
    minor_breaks = minor_breaks, labels = labels, limits = limits,
    expand = waiver(), oob = censor, na.value = NA_real_, trans = "identity",
    guide = "none",super = ScaleContinuousPosition
  )
  # TODO: Fix this hack. We're reassigning the parent ggproto object, but this
  # object should in the first place be created with the correct parent.
  #sc$super <- ScaleContinuousPosition
  #class(sc) <- class(ScaleContinuousPosition)
  sc
}


#' @rdname scale_X_continuous
#' @export
scale_L_continuous <- function(name         = waiver(), 
                               limits       = NULL,
                               breaks       = waiver(),
                               minor_breaks = waiver(),
                               labels       = waiver(),...) {
  sc <- continuous_scale(
    c("L"),
    "tern_L", identity, name = name, breaks = breaks,
    minor_breaks = minor_breaks, labels = labels, limits = limits,
    expand = waiver(), oob = censor, na.value = NA_real_, trans = "identity",
    guide = "none",super = ScaleContinuousPosition
  )
  # TODO: Fix this hack. We're reassigning the parent ggproto object, but this
  # object should in the first place be created with the correct parent.
  #sc$super <- ScaleContinuousPosition
  #class(sc) <- class(ScaleContinuousPosition)
  sc
}

#' @rdname scale_X_continuous
#' @export
scale_R_continuous <- function(name         = waiver(), 
                               limits       = NULL,
                               breaks       = waiver(),
                               minor_breaks = waiver(),
                               labels       = waiver(),...) {
  sc <- continuous_scale(
    c("R"),
    "tern_R", identity, name = name, breaks = breaks,
    minor_breaks = minor_breaks, labels = labels, limits = limits,
    expand = waiver(), oob = censor, na.value = NA_real_, trans = "identity",
    guide = "none",super = ScaleContinuousPosition
  )
  # TODO: Fix this hack. We're reassigning the parent ggproto object, but this
  # object should in the first place be created with the correct parent.
  #sc$super <- ScaleContinuousPosition
  #class(sc) <- class(ScaleContinuousPosition)
  sc
}
