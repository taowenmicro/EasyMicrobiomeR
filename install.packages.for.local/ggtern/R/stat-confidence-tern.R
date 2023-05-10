#' Confidence Interval
#' 
#' Statistic
#' @param contour If \code{TRUE}, contour the results of the 2d density estimation
#' @param n number of grid points in each direction
#' @param h Bandwidth (vector of length two). If \code{NULL}, estimated using \code{\link[MASS]{bandwidth.nrd}}.
#' @section Computed variables: 
#' Same as \code{\link{stat_contour}}
#' @rdname geom_confidence_tern
#' @export
stat_confidence_tern <- function( mapping  = NULL, data = NULL, geom = "ConfidenceTern", position = "identity", 
                                  ...,
                                  contour = TRUE, n = 100, h = NULL, na.rm = FALSE, breaks = c(0.50,0.90,0.95), 
                                  show.legend = NA, inherit.aes = TRUE) {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = StatConfidenceTern,
    geom        = geom,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      na.rm     = na.rm,
      contour   = contour,
      n         = n,
      breaks    = breaks,
      ...
    )
  )
}

#' @rdname geom_confidence_tern
#' @format NULL
#' @usage NULL
#' @export
stat_confidence <- function(...){
  tern_dep('1.0.6.1',"Depreciated due to naming package naming standards, replaced by stat_confidence_tern")
  stat_confidence_tern(...)
}



#' @rdname geom_confidence_tern
#' @format NULL
#' @usage NULL
#' @export
StatConfidenceTern <- ggproto("StatConfidenceTern", 
  Stat, 
  retransform  = FALSE,
  required_aes = c("x","y","z"),
  compute_group = function(self,data, scales, na.rm = FALSE, h = NULL, contour = TRUE, n = 100, breaks=c(0.50,0.90,0.95)) {
    
    #Check required aesthetics
    ggint$check_required_aesthetics(self$required_aes, names(data), ggint$snake_class(self))
    
    #Start with an initially empty dataframe
    ret    = data.frame()
    
    #Try and execute the computation
    tryCatch({
      
      #Remove incomplete rows
      data = remove_missing(data,vars=self$required_aes,na.rm=na.rm,name=class(self)[1],finite=TRUE)
      
      #Check that there is data to process
      if(length(breaks) == 0 | !is.numeric(breaks)) return(ret)
      
      #Merge the breaks with the data frame, if it hasnt been added
      if(!'breaks' %in% names(data)){ 
        breaks = sort(breaks,decreasing=TRUE)
        data   = merge(data,data.frame(breaks),all=TRUE,sort=T) 
      }
      
      #Split and process for each panel, group and break
      ret = ddply(data,c("PANEL","group","breaks"),function(df){
        #if(nrow(df) <= 1) return(data.frame)
        ix    = c('x','y','z')
        z     = ilr(as.matrix(df[,ix]) ) #Isometric Log Ratio Transform
        z     = z[is.finite(z[,1]) & is.finite(z[,2]),,drop=FALSE]
        
        if(!nrow(z) || !ncol(z)) 
          return(data.frame())
        
        mu    = colMeans(z); #Mean
        cm    = cov(z)       #Coveriance Matrix
        if( any(!is.finite(mu)) | any(!is.finite(cm)) ) 
          return(data.frame())
        dat   = mahalanobis_distance(z, mu, cm, whichlines=unique(df$breaks),m=n)
        
        xp1   = dat$mdX[,1]; yp1   = dat$mdY[,1] #1 index for 1 break
        inv   = ilrInv(cbind(xp1,yp1)) #Inverse Isometric Log Ratio
        
        loc   = data.frame(inv); names(loc) = ix
        ifthenelse(nrow(loc) > 2, rbind(loc,loc[1,,drop=FALSE]),loc)
      })
      
      #Add the missing but required 'piece' and 
      ret$piece = as.integer(.reverse.factor(ret$breaks))
      ret$group = factor(paste(ret$group,ret$piece,sep="-"))
      ret       = rename(ret,c("breaks"="level"))
      
    },error=function(e){ warning(e) })
    
    #Done
    ret
  }
)

#Internals
.reverse.factor <- function(x){ x = factor(x); x = factor(x,levels=rev(levels(x))); x }
