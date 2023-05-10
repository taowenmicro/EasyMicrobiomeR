.onLoad <- function(libname, pkgname){

  #Set the options
  .setOptionsCurrent()
  .setOptionsDepreciated()
  
  #Set the theme and the last coordinates.
  theme_set(theme_gray())
  ggtern_register_theme_elements();
}

.onAttach <- function(libname, pkgname){
  lines = c("--",
            sprintf("Remember to cite, run citation(package = '%s') for further info.",pkgname),
            "--")
  msg = paste(lines,collapse="\n")
  packageStartupMessage(msg)
}

#------------------------------------------------------------------------------
#CURRENT OPTIONS
#------------------------------------------------------------------------------
.setOptionsCurrent <- function(){
  options("tern.expand"                = 0.2)
  options('tern.margin'                = unit(0,'pt'))
  options('tern.arrow'                 = arrow(length=unit(2.5,"mm")))
  options("tern.default.T"             = "y")
  options("tern.default.L"             = "x")
  options("tern.default.R"             = "z")
  options("tern.clockwise"             = FALSE)
  options("tern.title.show"            = TRUE)
  options("tern.text.show"             = TRUE)
  options("tern.axis.ontop"            = FALSE)
  options("tern.arrow.start"           = 0.3)
  options("tern.arrow.finish"          = 0.7)
  options("tern.arrow.show"            = FALSE)
  options('tern.arrow.sep'             = 0.1)
  options('tern.vshift'                = 0.0)
  options('tern.hshift'                = 0.0)
  options("tern.ticks.outside"         = TRUE)
  options("tern.ticks.primary.show"    = TRUE)
  options("tern.ticks.secondary.show"  = FALSE)
  options("tern.breaks.default"        = seq(0.0, 1.0,by=0.2))
  options("tern.breaks.default.minor"  = seq(0.1, 0.9,by=0.2))
  options("tern.grid.major.show"       = TRUE)
  options("tern.grid.minor.show"       = TRUE)
  options("tern.grid.ontop"            = FALSE)
  options("tern.mask.show"             = TRUE)
  options("tern.mask.debug"            = FALSE)
  options("tern.rotate"                = 0)
  options("tern.latex"                 = FALSE)
}

#------------------------------------------------------------------------------
#DEPRECIATED OPTIONS -- ie either not used anymore or in depreciated functions.
#------------------------------------------------------------------------------
.setOptionsDepreciated <- function(){
  options("tern.discard.external"      = TRUE)
  options("tern.expand.contour.inner"  =-0.0005)
  options("tern.dont_transform"        = FALSE)
  options("tern.mesh.buffer"           = 1.50)
  options("tern.mesh.size"             = 200)
}


#------------------------------------------------------------------------------
#MANUAL EXECUTION -- BUILD STATICDOCS
#------------------------------------------------------------------------------
if(FALSE){
  library(grid)
  # install.packages("devtools")
  devtools::install_github("hadley/staticdocs",force=TRUE)
  library(devtools)
  source("./inst/staticdocs/icons.R")
  buildStaticDocs = function(){
    
    build_site = function(pkg = "."){
      pkg = staticdocs::as.sd_package(pkg)
      inputRMD = sprintf("%s/README.rmd",pkg$sd_path)
      if(file.exists(inputRMD))
        knitr::knit(input=inputRMD,output=sub("rmd$",'md',inputRMD,perl=TRUE))
      invisible(staticdocs::build_site(pkg))
    }
    
    build_demos = function (pkg = ".") {
      pkg <- staticdocs::as.sd_package(pkg)
      demo_dir <- file.path(pkg$path, "demo")
      if (!file.exists(demo_dir)) 
        return()
      message("Rendering demos")
      demos <- readLines(file.path(demo_dir, "00Index"))
      pieces <- stringr::str_split_fixed(demos, "\\s+", 2)
      in_path <- stringr::str_c(pieces[, 1], ".r")
      filename <- stringr::str_c("demo-", pieces[, 1], ".html")
      title <- pieces[, 2]
      for (i in seq_along(title)) {
        demo_code <- readLines(file.path(demo_dir, in_path[i]))
        demo_expr <- evaluate::evaluate(demo_code, new.env(parent = globalenv()), new_device = FALSE)
        
        #NH: replay_html is not exported...
        replay_html <- getFromNamespace('replay_html','staticdocs')
        
        pkg$demo      <- replay_html(demo_expr, pkg = pkg, name = stringr::str_c(pieces[i],"-"))
        pkg$pagetitle <- sprintf("Demo: %s",title[i])
        
        #NH: Need to set the title attribute...
        pkg$title     <- pkg$pagetitle
        
        staticdocs::render_page(pkg, "demo", pkg, file.path(pkg$site_path, filename[i]))
      }
      invisible(list(demo = unname(apply(cbind(filename,title), 1, as.list))))
    }
    
    #Function to build sitemap
    build_sitemap = function(pkg=".",destination = NULL){
      pkg        <- as.sd_package(pkg)
      site_path <- pkg$site_path
      message("Rendering Sitemap")
      if(!dir.exists(site_path)) 
        build_site(pkg)
      
      #THE XML TEMPLATE
      tpl <- '<?xml version="1.0" encoding="UTF-8"?>
<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
{{#links}}
<url>
<loc>{{{loc}}}</loc>
<lastmod>{{{lastmod}}}</lastmod>
<changefreq>{{{changefreq}}}</changefreq>
<priority>{{{priority}}}</priority>
</url>
{{/links}}
</urlset>'
      
      #Map all the html files to xml records
      if(is.null(destination))
        destination = sprintf('%s/d/%s',pkg$urls[1],packageVersion("ggtern"))
      links <- list.files(site_path,pattern = 'html$')
      map_links <- function(l,base = baseurl,live=FALSE) {
        l   <- sprintf("%s/%s",destination,l)
        d   <- if(live) httr::GET(l)$headers[['last-modified']] else lubridate::now()
        list(loc        = l,
             lastmod    = format(as.Date(d,format="%a, %d %b %Y %H:%M:%S")),
             changefreq = "monthly",
             priority   = "0.8")
      }
      links       <- lapply(links, map_links)
      filename    <- "sitemap.xml"
      output_file <- sprintf("%s/%s",site_path,filename)
      
      message(sprintf("generating: %s",filename))
      invisible(cat(whisker::whisker.render(tpl),file = output_file))
      }
    
    #Make the site and the sitemap
    build_site(pkg = ".")
    build_demos(pkg = ".")
    build_sitemap(pkg = ".")
  }
  buildStaticDocs()
}

#------------------------------------------------------------------------------
#Generate @params for theme
#------------------------------------------------------------------------------
if(FALSE){
  el = ggtern:::ggint$.element_tree
  el.ix = names(formals(theme)); el.ix = el.ix[grep(el.ix,pattern='tern')]
  result = unlist(lapply(el.ix,function(x){
    e = el[[x]]; print(e)
    s = `if`(is.character(e$inherit),sprintf('; inherits from `%s`',e$inherit),'')
    sprintf("#' @param %s %s (`%s`%s)",x,e$description,e$class,s)
  }))
  writeLines(result)
}

#------------------------------------------------------------------------------
#Unit Checks
#------------------------------------------------------------------------------
if(FALSE){
  #Check there are no new theme elements
  all(names(formals(ggplot2::theme)) %in% 
      names(formals(ggtern::theme)))
}



