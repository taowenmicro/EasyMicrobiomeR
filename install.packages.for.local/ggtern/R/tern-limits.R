#' Restrict Ternary Limits
#' 
#' \code{tern_limits} (or its aliasses) appends new \code{T}, \code{L} and \code{R} ternary continuous scales, 
#' where the maximum scale value is specified, and, where the minimums for each are solved.
#' 
#' The contra value (ie minimum value) for the \code{T}, \code{L} and \code{R} species is solved using
#' linear equations, therefore, if the solution is degenerate, or, the solution results in a zero range in either
#' of the proposed scales, then a warning message will be reported and an empty list returned. Note that 
#' \code{limits_tern(\dots), limit_tern(\dots)} and \code{tern_limit(\dots)} are all aliasses for 
#' the main function, \code{tern_limits(\dots)} and can be used interchangeably.
#' 
#' @return Either an empty list (when no solution can be found), or a list containing one of each 
#' of \code{scale_X_continuous} (\code{X = T, L, R})
#' 
#' @param T,L,R numeric value (scalar) of the maximum \code{T,L,R} species limit for each scale respectively
#' @param ... other arguments to pass to ALL of \code{scale_X_continuous} (\code{X = T, L, R})
#' 
#' @seealso \code{\link{scale_T_continuous}}, \code{\link{scale_L_continuous}} and \code{\link{scale_R_continuous}}
#' @examples 
#' #Display a non-zoomed and zoomed plot side by side
#' data(Feldspar)
#' df.lims = data.frame(Ab = c(1,.25,.25), 
#'                      An = c(0,.75,.00), 
#'                      Or = c(0,.00,.75))
#' #Build the non-zoomed plot
#' A = ggtern(Feldspar,aes(Ab,An,Or)) +
#'  stat_density_tern(geom='polygon',aes(fill=..level..,alpha=..level..)) + 
#'  geom_point() + 
#'  geom_mask() + 
#'  geom_polygon(data=df.lims,color='red',alpha=0,size=0.5) +
#'  guides(color='none',fill='none',alpha='none') + 
#'  labs(title = "Non-Zoomed")
#' 
#' #Build the zoomed plot
#' B = A + 
#'   tern_limits(T=max(df.lims$An), L=max(df.lims$Ab), R=max(df.lims$Or)) +
#'   labs(title = "Zoomed")
#' 
#' #Arrange the above plots side by side for illustration
#' grid.arrange(A,B,ncol=2,top="Demonstration of Limiting Region")
#' @author Nicholas Hamilton
#' @name tern_limits
#' @rdname tern_limits
NULL

#' @rdname tern_limits
#' @export
tern_limit <- function(T=1,L=1,R=1,...){

  #Run Check on input variables
  if(!all(sapply(list(T,L,R),function(x){length(x) == 1 && is.numeric(x)})))
    stop("Arguments T, L and R must be numeric and scalar",call.=FALSE)
  
  ret <- list()
  tryCatch({
    
    #Put the argments in a list, to pass on
    args = list(...)
    
    #Solve the linear equations Ax = B
    A    <- diag(-1,3,3) + 1  #All 1's and 0's down diagonal
    B    <- c( 1-T ,1-L ,1-R)
    x    <- round(solve(A,B),3)
    lims <- list(T = sort(c(x[1],T)), 
                 L = sort(c(x[2],L)), 
                 R = sort(c(x[3],R)))
    
    #Run some checks
    if(any(sapply(lims,diff) == 0))
      stop("Invalid limits, solution produces zero ranges on some scales",call.=FALSE)
    if(any(sapply(lims,max) > 1 | sapply(lims,min) < 0))
      warning("Solution to limits produces range outside of [0,1] for some scales",call.=FALSE)
    if('limits' %in% names(args))
      warning("Explicit 'limits' argument will be discarded by calculated values.",call.=FALSE)
    
    #Funcion to construct the scales
    scaleX = function(X,a = args){
      a$limits = lims[[X]]
      do.call(sprintf("scale_%s_continuous",X),a)
    }
    
    #Build the collection of scales
    ret <- list( scaleX('T'), scaleX('L'), scaleX('R'))
    
  },error=function(e){ warning(e)  })
  invisible(ret)
}

#'@rdname tern_limits
#'@export 
limit_tern <- function(...) tern_limit(...) 

#'@rdname tern_limits
#'@usage NULL
#'@format NULL
#'@export 
limits_tern <- function(...) tern_limit(...)

#'@rdname tern_limits
#'@usage NULL
#'@format NULL
#'@export 
tern_limits <- function(...) tern_limit(...) 


