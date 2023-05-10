emptyPlot <- function(data=NULL,mapping=aes()){
  ggtern(data,mapping) + 
    theme_bw() + 
    theme_nolabels() + 
    theme_noarrows() + 
    theme_notitles() +
    theme_nogrid() +
    theme_noticks() + 
    theme(tern.panel.expand=0.05,panel.border=element_blank())
}

coord_tern  <- sd_icon({
  suppressMessages(require(grid))
  gTree(children = gList(
    ggplotGrob(emptyPlot()),
    segmentsGrob(
      c(0, 0.25),
      c(0.25, 0),
      c(1, 0.25),
      c(0.25, 1),
      gp = gpar(col = "grey50", lwd = 0.5)
    ),
    segmentsGrob(
      c(0, 0.75),
      c(0.75, 0),
      c(1, 0.75),
      c(0.75, 1),
      gp = gpar(col = "grey50", lwd = 0.5)
    ),
    segmentsGrob(c(0, 0.5), c(0.5, 0), c(1, 0.5), c(0.5, 1))
  ))
})

scale_X_continuous <- sd_icon({
  suppressMessages(require(grid))
  suppressMessages(require(ggtern))
  x = emptyPlot() + 
      theme_showprimary() + 
      theme_novar_tern(c("R","L")) +
      theme(tern.axis.ticks.length.major = unit(5,'mm')) +
      theme(tern.axis.hshift=-0.25)
  ggplotGrob(x)
})

tern_limits <- sd_icon({
  df = data.frame(x=c(.7,.15,.15),y=c(.15,.7,.15),z=c(.15,.15,.7))
  x = emptyPlot(df,aes(x,y,z)) + 
    geom_polygon(fill='red',alpha=.5,color='black') +
    geom_point(size=1)
  ggplotGrob(x)
})

approved_layers <- sd_icon({
  g = textGrob(label="?",gp = gpar(fontsize=30))
  gTree(children=gList(g))
})

geom_crosshair_tern <- sd_icon({
  df = data.frame(x=c(1),y=c(1),z=c(1))
  x = emptyPlot(df,aes(x,y,z)) + 
    geom_crosshair_tern(color='magenta',size=0.5) +
    geom_point(size=1)
  ggplotGrob(x)
})

geom_confidence_tern <- sd_icon({
  data(Feldspar)
  x = emptyPlot(Feldspar,aes(Ab,An,Or)) + 
    geom_confidence_tern(size=0.5,color='black')
  ggplotGrob(x)
})

geom_density_tern <- sd_icon({
  data(Feldspar)
  x = emptyPlot(Feldspar,aes(Ab,An,Or)) + 
    geom_density_tern(h=1.5,bins=5,size=0.5,aes(color=..level..)) +
    scale_color_gradient(low='blue',high='red') + guides(color='none')
  ggplotGrob(x)
})

geom_interpolate_tern <- sd_icon({
  data(Feldspar)
  x = emptyPlot(Feldspar,aes(Ab,An,Or)) + 
    geom_interpolate_tern(aes(value=T.C),base='identity',
                          bins=1,n=200,color='red',size=0.5,
                          expand=1,method=lm,formula=value~poly(x,y,degree=3))
  ggplotGrob(x)
})

geom_errorbarX <- sd_icon({
  df = data.frame(x=1,y=1,z=1)/3
  x = emptyPlot(df,aes(x,y,y)) + 
    geom_errorbarT(Tmax=1.75*df$x,Tmin=0.25*df$x,color='red',
                   size=.5,
                   arrow=arrow(angle=90,length=unit(.1,'npc'))) +
    geom_point(size=1)
  ggplotGrob(x)
})

geom_Xisoprop <- sd_icon({
  x = emptyPlot() + 
    geom_Tisoprop(value=c(.25,.5,.75),color='magenta',size=0.5)
  ggplotGrob(x)
})

geom_smooth_tern <- sd_icon({
  data(Feldspar)
  x = emptyPlot(data=Feldspar,aes(Ab,An,Or)) + 
    geom_smooth_tern()
  ggplotGrob(x)
})

geom_Xline <- sd_icon({
  x = emptyPlot() + 
    geom_Lline(Lintercept=c(.25,.5,.75),color='orange',size=0.5)
  ggplotGrob(x)
})

geom_point_swap <- sd_icon({
  df = data.frame(x=1,y=1,z=1)/3
  x = emptyPlot(df,aes(x,y,z)) + 
    geom_mask() + 
    geom_point_swap(fill='red',color='blue',size=6,shape=21,alpha=.5,position = position_nudge_tern(-.15,+.15,0)) +
    geom_point(     fill='red',color='blue',size=6,shape=21,alpha=.5,position = position_nudge_tern(+.15,-.15,0))
  ggplotGrob(x)
})

geom_mask <- sd_icon({
  x = emptyPlot() + 
    geom_mask() + 
    theme(tern.plot.background = element_rect(fill=alpha('red',.5)))
  ggplotGrob(x)
})

annotate <- sd_icon({
  x = emptyPlot() + 
    geom_mask() + 
    annotate('label',x=1,y=1,z=1,label='xyz',color='black',size=2.5,alpha=0.25,fill='blue')
  ggplotGrob(x)
})

annotation_raster_tern <- sd_icon({
  data(Feldspar)
  data(FeldsparRaster)
  x = emptyPlot(Feldspar,aes(Ab,An,Or)) + 
    annotation_raster_tern(FeldsparRaster,xmin=0,xmax=1,ymin=0,ymax=1) +
    stat_confidence_tern(geom='polygon',fill='red',alpha=0.2,aes(group=Feldspar))
  ggplotGrob(x)
})

