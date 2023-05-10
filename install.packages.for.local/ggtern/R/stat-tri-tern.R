#' @export
#' @rdname geom_tri_tern
#' @inheritParams ggplot2::stat_bin_2d
#' @param centroid logical to return the centroid of the polygon, rather than the complete polygon
#' @export
stat_tri_tern <- function(mapping = NULL, data = NULL,
                          geom = "tri_tern", position = "identity",
                          ...,
                          bins = 30,
                          fun = sum,
                          centroid = FALSE,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = StatTriTern,
    geom        = geom,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      bins      = bins,
      na.rm     = na.rm,
      fun       = fun,
      centroid  = centroid,
      ...
    )
  )
}

#' @rdname geom_tri_tern
#' @format NULL
#' @usage NULL
#' @export
StatTriTern <- ggproto("StatTriTern", Stat,
    default_aes = aes(value = 1, fill = ..stat..),
    required_aes = c("x", "y","z"),
    setup_data = function(self,data,params){
      ##Ensure it is simplex
      raes        = self$required_aes
      data[,raes] = as.data.frame(acomp(data[,raes]))
      data
    },
    compute_group = function(self, data, scales, bins = 30, na.value = NA, na.rm = FALSE, fun = sum, centroid = FALSE) {
      
      ##For Consistency with ggplo2 hexbin
      value       = rep(1,nrow(data))
      bin         = triBinSummarise(data$x, data$y, data$z, value, bins, sum, na.value = na.value)
      bin$density = as.vector(bin$value / sum(bin$value, na.rm = TRUE))
      
      ##User Defined Stats
      value       = data$value %||% value
      out         = triBinSummarise(data$x, data$y, data$z, value, bins, fun, na.value = na.value)
      
      #Assemble
      out$group   = out$IDPolygon
      out$stat    = out$value
      out$count   = bin$value
      out$density = bin$density
      out$value   = out$IDPolygon = NULL
      
      #Remove NA's
      if(na.rm){
          out         = remove_missing(out,FALSE,
                                       c(self$required_aes, 
                                         c('stat','count','density')),
                                       ggint$snake_class(self),
                                       finite = TRUE)
      }
      
      #If Centroid
      if(centroid){
        out = ddply(out,'group',function(df){
          res = df[1,,drop=FALSE]
          res[,self$required_aes] = colMeans(df[,self$required_aes])
          res
        })
      }
      
      out
    }
)

triMesh <- function(n = 1) {
  n <- as.integer(max(n[1], 1))
  temp <- seq(0, n, 1)
  df <- data.frame(
    x = unlist(sapply((n+1):1, function(i) temp[1:i])),
    y = rep(0:n, (n+1):1)
  )
  df$z <- n - df$x - df$y
  df <- cbind(0:(nrow(df)-1), df / n)
  names(df) <- c('IDPoint', 'x', 'y', 'z')
  return(df)
}

triPoly = function( n = 1 ){
  n     = as.integer(max(n[1],1))
  mesh  = triMesh( n )
  inv   = 1/n
  
  #Triangles: A complete space-filling motiv contains a
  #           Upper and a Lower Triangle at each point.
  #           These triangles are diagonally opposed
  result = with(mesh,ddply(mesh,.(IDPoint),function(df){
    x = df$x; y=df$y; p = df$IDPoint[1]
    
    #Upper Triangles
    a = data.frame(x = c(x,x,x+inv),
                   y = c(y,y+inv,y))
    a$z = with(a,1-x-y); mn = min(a); mx = max(a);
    a$IDPoint = p
    if(mn < -inv/2 || mx > 1 + inv/2)
      a = a[0,,drop=FALSE]
    
    #Lower Triangles
    b = data.frame(x=c(x,x,x-inv),
                   y=c(y,y-inv,y))
    b$z = with(b,1-x-y); mn = min(b); mx = max(b);
    b$IDPoint = -p
    if(mn < -inv/2 || mx > 1 + inv/2) 
      b = b[0,,drop=FALSE]
    
    #Done
    rbind(a,b)[,c('IDPoint','x','y','z')]
  }))
  

  names(result)[1] = "IDPolygon"
  result
}

triStat <- function(df, xmin = 0, xmax = 1, ymin = 0, ymax = 1, zmin = 0, zmax = 1,na.value=NA,fun) {
  df <- with(df, df[xmin <= x & x < xmax &
                    ymin <= y & y < ymax &
                    zmin <= z & z < zmax  ,,drop=F])
  #If No rows, return NA
  nr  <- nrow(df)
  if(is.na(nr) || nr == 0) return(na.value[1])
  with(df,fun(w))
}


triBinSummarise = function(x,y,z,w,bins,fun, fun.args=list(), na.value=NA, drop = TRUE){
  poly    = triPoly(bins)
  theBins = with(poly,ddply(poly, .(IDPolygon), here(summarize), 
                            xmin = min(x), xmax = max(x), 
                            ymin = min(y), ymax = max(y), 
                            zmin = min(z), zmax = max(z))
                 )
  df   = data.frame(x,y,z,w)
  stat = with(theBins,ddply(theBins, .(IDPolygon), here(summarize), 
                            value = triStat(df, xmin, xmax, ymin, ymax, zmin, zmax, na.value, fun)))
  merge(poly,stat)
}
