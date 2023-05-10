#' Plot Construction
#' 
#' \code{"+.gg"} is a local copy of the ggplot2 add function, no change other than exporting from the ggtern namespace
#' @param e1 first object
#' @param e2 second object
#' @author Nicholas Hamilton
#' @rdname plot_construction
#' @export
# "+.gg" <- function(e1, e2){  
#  # Get the name of what was passed in as e2, and pass along so that it
#  # can be displayed in error messages
#  e2name <- deparse(substitute(e2))
#  if      (is.theme(e1))  add_theme( e1, e2, e2name)
#  else if (is.ggplot(e1)) add_ggplot(e1, e2, e2name)
#}

#' @rdname plot_construction
#' @export
#"%+%" <- `+.gg`

#add_ggplot <- function(p, object, objectname) {
#     if (is.null(object)) return(p)
#     
#     p <- ggint$plot_clone(p)
#     if (is.data.frame(object)) {
#       p$data <- object
#     } else if (is.theme(object)) {
#       p$theme <- update_theme(p$theme, object)
#     } else if (inherits(object, "Scale")) {
#       p$scales$add(object)
#     } else if (inherits(object, "labels")) {
#       p <- ggint$update_labels(p, object)
#     } else if (inherits(object, "guides")) {
#       p <- ggint$update_guides(p, object)
#     } else if (inherits(object, "uneval")) {
#       p$mapping <- defaults(object, p$mapping)
#       # defaults() doesn't copy class, so copy it.
#       class(p$mapping) <- class(object)
#       
#       labels <- lapply(object, deparse)
#       names(labels) <- names(object)
#       p <- update_labels(p, labels)
#     } else if (ggint$is.Coord(object)) {
#       p$coordinates <- object
#       p
#     } else if (ggint$is.facet(object)) {
#       p$facet <- object
#       p
#     } else if (is.list(object)) {
#       for (o in object) {
#         p <- p %+% o
#       }
#     } else if (ggint$is.layer(object)) {
#       p$layers <- append(p$layers, object)
#       
#       # Add any new labels
#       mapping <- ggint$make_labels(object$mapping)
#       default <- ggint$make_labels(object$stat$default_aes)
#       new_labels <- defaults(mapping, default)
#       p$labels <- defaults(p$labels, new_labels)
#     } else {
#       stop("Don't know how to add ", objectname, " to a plot",
#            call. = FALSE)
#     }
#     ggint$set_last_plot(p)
#     p
# }