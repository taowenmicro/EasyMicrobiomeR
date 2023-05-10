# Documentation

*ggtern, An extension to ggplot2, for the creation of ternary diagrams.*

`ggtern` is a package that extends the functionality of [ggplot2](http://goo.gl/YDk79h), giving the capability to plot ternary diagrams for (subset of) the ggplot2 proto geometries. Ternary diagrams are used frequently in a number of disciplines to graph compositional features for mixtures of three different elements or compounds. It is possible to represent a coordinate system having three (3) degrees of freedom, in 2D space, since the third dimention is linear and depends only on the other two. 

`ggtern` is a package that is based on (extends) the very popular ggplot2, which is an implementation of Wilkinsons [The Grammar of Graphics](http://goo.gl/G5DEo3),  and, makes provision for a highly methodical construction process for the development  of meaningful (graphical) data representations. Of course, the above book by Wilkinson outlines the theory, whilst Hadley Wickhams ggplot2 implementation is where much of the magic happens, and, an ideal base-platform for `ggtern`.

## Installation

Install the latest release on CRAN, install just like any other R package:

```
  install.packages('ggtern')
```

To install the development / working version, use the `devtools` package:

```
  devtools::install_git('https://bitbucket.org/nicholasehamilton/ggtern')
```

## Citations

Please cite this package as follows:


```r
citation('ggtern')
```

```
## 
## To cite package 'ggtern' in publications use:
## 
##   Nicholas Hamilton (2018). ggtern: An Extension to 'ggplot2', for
##   the Creation of Ternary Diagrams. R package version 2.2.2.
##   https://CRAN.R-project.org/package=ggtern
## 
## A BibTeX entry for LaTeX users is
## 
##   @Manual{,
##     title = {ggtern: An Extension to 'ggplot2', for the Creation of Ternary Diagrams},
##     author = {Nicholas Hamilton},
##     year = {2018},
##     note = {R package version 2.2.2},
##     url = {https://CRAN.R-project.org/package=ggtern},
##   }
```

