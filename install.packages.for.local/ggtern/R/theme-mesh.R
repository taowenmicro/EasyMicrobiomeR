#' Create Grid Mesh
#' 
#' Convenience function for creation of a grid mesh of an ideal number of 'n' major breaks. Note that 
#' the value of 'n' is the target number of breaks, and due to the use of the \code{\link{pretty}} function
#' within \code{\link{breaks_tern}} convenience function, may not be strictly adhered or reflected.
#' @param n the 'target' number of major breaks
#' @param ... additional arguments to be passed through to \code{\link{tern_limits}}
#' @examples 
#' #Default example of a target n=10 mesh
#' ggtern() + 
#'  theme_mesh(10)
#' 
#' #Default example, of a target n=5 mesh, with limiting region
#' ggtern() + 
#'  theme_mesh(5,T=.5,L=.5,R=.5)
#' @author Nicholas Hamilton
#' @rdname theme_mesh
#' @export
theme_mesh = function(n = 5,...){
  
  #Check the value of n is valid
  if(!is.numeric(n)) 
    stop("'n' must be numeric")
  n = max(as.integer(n[1]),1)
  
  # Build the args from the ellipsis
  # Warn (and reset) if the elipsis contains items to be calculated
  args   = list(...)
  items  = c('breaks','minor_breaks')
  for(ix in seq_along(items)){
    item = items[ix]
    if(item %in% names(args)){
      warning(sprintf("Argument '%s' has been provided and will be replaced",item),call.=FALSE)
      args[[item]] = NULL
    }
  }
  
  #Get the base scales from call on tern_limits function
  scales = do.call(tern_limits,args=args)
  
  #Now modify each scale, setting the major and minor breaks.
  reset  = FALSE
  scales = lapply(scales,function(sc){
    
    #Get the local limits and labels
    limits = sc$limits; labels = sc$labels
    
    # Inject the calculated major and minor breaks
    for(ix in seq_along(items))
      sc[[ items[ix] ]] = breaks_tern(limits = limits, isMajor = (ix == 1) , n = n)
    
    # Check labels are valid for major breaks, reset if not.
    if(!inherits(labels,'waiver') && !is.null(labels)){
      if(length(sc$breaks) %% length(labels) != 0 || reset){
        reset <<- TRUE; sc$labels = waiver()
      }
    }
    
    #Done, Return from lapply function
    sc
  })
  
  #Warn if some of the labels have been reset to the default values
  if(reset) 
    warning("One or more of the scales had invalid label lengths and have been reset.",call.=FALSE)
  
  #Done, Return
  scales
}