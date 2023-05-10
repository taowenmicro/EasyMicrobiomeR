#' Atomic, Weight or Custom Percentage Suffix
#' 
#' By default there are no suffixes behind the arrow label marker (the arrow up next to the ternary axes), 
#' and these functions appends to the set of arrow labels, a value to indicate the nature of the scale.
#' 
#' These are convenience wrappers to \code{labs(W="XYZ")}.
#' @seealso Convenience functions for \code{\link[=Tlab]{T, L, R, W labels}}
#' @author Nicholas Hamilton
#' @rdname ggtern_labels_arrow_suffix
#' @name   ggtern_labels_arrow_suffix
#' 
NULL

#' @description \code{percent_weight} adds 'Wt. \%' to the arrow marker label as a suffix
#' @rdname ggtern_labels_arrow_suffix
#' @export
percent_weight <- function(){labs(W="Wt.%")}

#' @description \code{weight_percent} is an alias for \code{percent_weight()}
#' @rdname ggtern_labels_arrow_suffix
#' @export
weight_percent <- percent_weight

#' @description \code{percent_atomic} adds 'At. \%' to the arrow marker label as a suffix
#' @rdname ggtern_labels_arrow_suffix
#' @export
percent_atomic <- function(){labs(W="At.%")}

#' @description \code{atomic_percent} is an alias for \code{percent_atomic()}
#' @rdname ggtern_labels_arrow_suffix
#' @export
atomic_percent <- percent_atomic

#' @description \code{percent_custom} adds a custom suffix to the arrow label marker.
#' @param x the custom suffix
#' @rdname ggtern_labels_arrow_suffix
#' @export
percent_custom <- function(x){
  if(is(x,'character')){
    x = gsub("%","%",x)
    x = gsub('([[:punct:]])\\1+', '\\1', x)
  }
  labs(W=x)
}

#' @description \code{custom_percent} is an alias for \code{percent_custom()}
#' @rdname ggtern_labels_arrow_suffix
#' @export
custom_percent <- percent_custom


