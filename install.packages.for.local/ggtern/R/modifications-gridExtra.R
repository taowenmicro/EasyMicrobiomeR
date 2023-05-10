#' Arrange multiple grobs on a page (ggtern version)
#' 
#' A very slight modification to the original function, removing the explicit direction to use the ggplotGrob function
#' from the ggplot2 namespace
#' @inheritParams gridExtra::arrangeGrob
#' @author Nicholas Hamilton
#' @rdname arrangeGrob
#' @export
arrangeGrob = function (..., grobs = list(...), layout_matrix, vp = NULL, name = "arrange", 
                        as.table = TRUE, respect = FALSE, clip = "off", nrow = NULL, 
                        ncol = NULL, widths = NULL, heights = NULL, top = NULL, bottom = NULL, 
                        left = NULL, right = NULL, padding = unit(0.5, "line")) 
{
  n <- length(grobs)
  if (!is.null(ncol) && !is.null(widths)) {
    stopifnot(length(widths) == ncol)
  }
  if (!is.null(nrow) && !is.null(heights)) {
    stopifnot(length(heights) == nrow)
  }
  if (is.null(ncol) && !is.null(widths)) {
    ncol <- length(widths)
  }
  if (is.null(nrow) && !is.null(heights)) {
    nrow <- length(heights)
  }
  if (is.null(nrow) && !is.null(ncol)) {
    nrow <- ceiling(n/ncol)
  }
  if (is.null(ncol) && !is.null(nrow)) {
    ncol <- ceiling(n/nrow)
  }
  stopifnot(nrow * ncol >= n)
  if (is.null(nrow) && is.null(ncol) && is.null(widths) && 
      is.null(heights)) {
    nm <- grDevices::n2mfrow(n)
    nrow = nm[1]
    ncol = nm[2]
  }
  inherit.ggplot <- unlist(lapply(grobs, inherits, what = "ggplot"))
  inherit.trellis <- unlist(lapply(grobs, inherits, what = "trellis"))
  if (any(inherit.ggplot)) {
    stopifnot(requireNamespace("ggplot2", quietly = TRUE))
    toconv <- which(inherit.ggplot)
    #grobs[toconv] <- lapply(grobs[toconv], ggplot2::ggplotGrob)
    grobs[toconv] <- lapply(grobs[toconv], ggplotGrob) ##NH
  }
  if (any(inherit.trellis)) {
    stopifnot(requireNamespace("lattice", quietly = TRUE))
    toconv <- which(inherit.trellis)
    grobs[toconv] <- lapply(grobs[toconv], ggint$latticeGrob)
  }
  if (missing(layout_matrix)) {
    positions <- expand.grid(t = seq_len(nrow), l = seq_len(ncol))
    positions$b <- positions$t
    positions$r <- positions$l
    if (as.table) 
      positions <- positions[order(positions$t), ]
    positions <- positions[seq_along(grobs), ]
  }
  else {
    cells <- sort(unique(as.vector(layout_matrix)))
    range_cell <- function(ii) {
      ind <- which(layout_matrix == ii, arr.ind = TRUE)
      c(l = min(ind[, "col"]), r = max(ind[, "col"]), t = min(ind[, 
                                                                  "row"]), b = max(ind[, "row"]))
    }
    positions <- data.frame(do.call(rbind, lapply(cells, 
                                                  range_cell)))
    ncol <- max(positions$r)
    nrow <- max(positions$b)
  }
  if (is.null(widths)) 
    widths <- unit(rep(1, ncol), "null")
  if (is.null(heights)) 
    heights <- unit(rep(1, nrow), "null")
  if (!is.unit(widths)) 
    widths <- unit(widths, "null")
  if (!is.unit(heights)) 
    heights <- unit(heights, "null")
  gt <- gtable(name = name, respect = respect, heights = heights, ##NH
               widths = widths, vp = vp)
  gt <- gtable_add_grob(gt, grobs, t = positions$t, b = positions$b, 
                        l = positions$l, r = positions$r, z = seq_along(grobs), 
                        clip = clip)
  if (is.character(top)) {
    top <- textGrob(top)
  }
  if (is.grob(top)) {
    h <- grobHeight(top) + padding
    gt <- gtable_add_rows(gt, heights = h, 0)
    gt <- gtable_add_grob(gt, top, t = 1, l = 1, r = ncol(gt), 
                          z = Inf, clip = clip)
  }
  if (is.character(bottom)) {
    bottom <- textGrob(bottom)
  }
  if (is.grob(bottom)) {
    h <- grobHeight(bottom) + padding
    gt <- gtable_add_rows(gt, heights = h, -1)
    gt <- gtable_add_grob(gt, bottom, t = nrow(gt), l = 1, 
                          r = ncol(gt), z = Inf, clip = clip)
  }
  if (is.character(left)) {
    left <- textGrob(left, rot = 90)
  }
  if (is.grob(left)) {
    w <- grobWidth(left) + padding
    gt <- gtable_add_cols(gt, widths = w, 0)
    gt <- gtable_add_grob(gt, left, t = 1, b = nrow(gt), 
                          l = 1, r = 1, z = Inf, clip = clip)
  }
  if (is.character(right)) {
    right <- textGrob(right, rot = -90)
  }
  if (is.grob(right)) {
    w <- grobWidth(right) + padding
    gt <- gtable_add_cols(gt, widths = w, -1)
    gt <- gtable_add_grob(gt, right, t = 1, b = nrow(gt), 
                          l = ncol(gt), r = ncol(gt), z = Inf, clip = clip)
  }
  gt
}

#' @inheritParams gridExtra::arrangeGrob
#' @inheritParams gridExtra::grid.arrange
#' @rdname arrangeGrob 
#' @aliases grid.arrange
#' @export
grid.arrange = function (..., newpage = TRUE) {
  if (newpage) 
    grid.newpage()
  g <- ggtern::arrangeGrob(...)
  grid.draw(g)
  invisible(g)
}
