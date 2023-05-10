.firstUpper <- function(s) {paste(toupper(substring(s, 1,1)), substring(s, 2), sep="")}

rd_theme <- function(){
  dic <- ggint$.element_tree[which(!ggint$.element_tree %in% ggint$.element_tree.orig)]
  nam <- names(dic)
  nolink <- c()
  paste(
    "\nBased on the \\code{ggplot2} existing structure (\\link[ggplot2]{theme}), the \\strong{NEW} individual theme elements for the ternary plot are as follows:",
    "\\tabular{lll}{",
    "\\strong{NAME} \\tab \\strong{OBJECT}/(INHERITS) \\tab \\strong{DESCRIPTION} \\cr",
    paste(sapply(nam,function(x){
      obj  = dic[[x]]
      paste("\\code{",x,"} \\tab \\code{",
            ifthenelse(obj$class %in% nolink,obj$class,paste0("\\link{",obj$class,"}")) ,
            "}",
            ifthenelse(!is.null(obj$inherit),"/(",""),"",
            obj$inherit,
            ifthenelse(!is.null(obj$inherit),")",""),
            " \\tab ",obj$description,sep="")
    }),collapse="\\cr "),
    "\n}\n"
  )
}

aesthetics <- function(x) {
  req_aes <- x$required_aes
  def_aes <- names(x$default_aes)
  def_aes <- setdiff(def_aes, req_aes)
  if (length(req_aes) == 0) {
    # Suppress warnings which occur when sorting NULL
    return(suppressWarnings(sort(names(x$default_aes))))
  }
  if (length(def_aes) == 0) {
    return(paste("\\strong{", sort(x$required_aes), "}",sep = ""))
  }
  return(c(paste("\\strong{", sort(x$required_aes), "}", sep = ""), sort(def_aes)))
}

find_subclass = function (super, class) {
  name <- paste0(super, ggint$camelize(class, first = TRUE))
  if (!exists(name)) {
    stop("No ", tolower(super), " called ", name, ".", call. = FALSE)
  }
  obj <- get(name)
  if (!inherits(obj, super)) {
    stop("Found object is not a ", tolower(super), ".", call. = FALSE)
  }
  obj
}

geom_aesthetics <- function(x) {
  aesthetics(find_subclass("Geom", x))
}

stat_aesthetics <- function(x) {
  aesthetics(find_subclass("Stat", x))
}

rd_aesthetics <- function(type, name) {
  obj <- switch(type,
                geom  = find_subclass("Geom", name),
                stat  = find_subclass("Stat", name),
                coord = find_subclass('Coord',name)
  )
  aes <- aesthetics(obj)
  
  paste("\\code{", type, "_", name, "} ",
        "understands the following aesthetics (required aesthetics are in bold):\n\n",
        "\\itemize{\n",
        paste("  \\item ", aes, collapse = "\n", sep = ""),
        "\n}\n", sep = "")
}

