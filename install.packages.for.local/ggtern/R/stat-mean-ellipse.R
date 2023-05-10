#' Mean Ellipses
#' 
#' @param steps the number of discretisation points to draw the ellipses
#' @param r a scaling of the half-diameters
#' @section Computed variables: 
#' Same as \code{\link{stat_contour}}
#' @examples
#' data(Feldspar)
#' ggtern(data=Feldspar,aes(Ab,An,Or)) +
#'   theme_bw() + 
#'   stat_mean_ellipse(geom='polygon',steps=500,fill='red',color='black') +
#'   geom_point()
#' @importFrom compositions mean.rmult idt idtInv clrvar2ilr
#' @rdname geom_mean_ellipse
#' @export
stat_mean_ellipse <- function( mapping  = NULL, data = NULL, geom = "MeanEllipse", position = "identity", 
                                  ...,
                                  steps = 72, r = 1, na.rm = FALSE, 
                                  show.legend = NA, inherit.aes = TRUE) {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = StatMeanEllipse,
    geom        = geom,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      na.rm     = na.rm,
      steps     = steps,
      r         = r,
      ...
    )
  )
}


#' @rdname geom_mean_ellipse
#' @format NULL
#' @usage NULL
#' @export
StatMeanEllipse <- ggproto("StatMeanEllipse", 
    Stat, 
    retransform  = FALSE,
    required_aes = c("x","y","z"),
    compute_group = function(self, data, scales, na.rm = FALSE, contour = TRUE, steps = 72, r = 1) {
      
      #Check required aesthetics
      ggtern:::ggint$check_required_aesthetics(self$required_aes, names(data), ggint$snake_class(self))
      
      #Start with an initially empty dataframe
      ret    = data.frame()
      
      tryCatch({
        #The Aesthetic Mappings
        vars = self$required_aes
        
        #Remove incomplete rows
        data = remove_missing(data,vars=vars,na.rm=na.rm,name=class(self)[1],finite=TRUE)
        
        #Constant Parms
        w    = seq(0,2*pi,length.out = steps[1] + 1)
        sw   = sin(w)
        cw   = cos(w)
        
        #Split and process for each panel, group and break
        ret = plyr::ddply(data,setdiff(names(data),vars),function(data){
          
          d    = compositions::acomp(data[,vars])
          data[,vars] = as.data.frame(d)
          mu   = mean(d,robust=FALSE)  #Mean
          sig  = compositions::var(d, robust=FALSE)  #Variance
          ei   = eigen(compositions::clrvar2ilr(sig),symmetric=TRUE)
          
          if( min(ei$values) / max(ei$values) < -1E-8) {
            msg = "Non positive Semidefinite Matrix used in Ellipses"
            warning(msg)
            print(list(problem=msg,var=var,eigen=ei))
          }
          
          rs = sqrt(abs(ei$values))*r[1]
          me = compositions::idt(mu)
          c1 = me[1]+rs[1]*ei$vectors[1,1]*sw + rs[2]*ei$vectors[1,2]*cw
          c2 = me[2]+rs[1]*ei$vectors[2,1]*sw + rs[2]*ei$vectors[2,2]*cw
          as.data.frame(compositions::idtInv(cbind(c1,c2),orig=mu))
        })
        
      },error=function(e){
        warning(e)
      })
      
      #Done
      return(ret)
    }
)
