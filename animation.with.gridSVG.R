1
2
3
4
5
6
7
8
9
10
11
12
13
14
15
16
17
18
19
20
21
22
23
24
25
26
27
28
29
30
31
32
33
34
35
36
37
38
39
40
41
42
43
44
45
46
47
48
49
50
51
52
53
54
55
56
library(gridSVG)
library(XML)
 
animateTrellis <- function(object, file='animatedSVG.svg',
                           duration=.1, step=2, show=TRUE){
  nLayers <- dim(object)
  stopifnot(nLayers>1)
  for (i in seq_len(nLayers)){
    p <- object[i]
    label <- p$condlevels[[1]][i]
    ##Create intermediate SVG files
    g <- grid.grabExpr(print(p, prefix=label),
                       name=label)
    if (i==1){ ## First frame
      ga <- animateGrob(g, group=TRUE,
                        visibility="hidden",
                        duration=duration, begin=step)
    } else if (i==nLayers){ ##Last Frame
      gg <- garnishGrob(g, visibility='hidden')
      ga <- animateGrob(gg, group=TRUE,
                        visibility="visible",
                        duration=duration, begin=step*(i-1))
    } else { ##any frame
      gg <- garnishGrob(g, visibility='hidden')
      gaV <- animateGrob(gg, group=TRUE,
                         visibility="visible",
                         duration=duration, begin=step*(i-1))
      ga <- animateGrob(gaV, group=TRUE,
                        visibility="hidden",
                        duration=duration, begin=step*i)
    }
    grid.newpage()
    grid.draw(ga)
    fich <- tempfile(fileext='.svg')
    gridToSVG(fich)
 
    ## Combine all
    if (i==1) {
      svgTop <- xmlParse(fich)
      nodeTop <- getNodeSet(svgTop,
                            "//svg:g[@id='gridSVG']",
                            c(svg="http://www.w3.org/2000/svg"))[[1]]
    } else {
      svgChildren <- xmlParse(fich)
      node <- getNodeSet(svgChildren,
                         "//svg:g[@id='gridSVG']/*",
                         c(svg="http://www.w3.org/2000/svg"))
      addChildren(nodeTop, node)
    }
    unlink(fich)
  }
  saveXML(svgTop, file=file)
  dev.off()
  if (show) browseURL(file)
  invisible(svgTop)
}

#-------------------------

trellis.device(pdf, file='iris.pdf')
p <- xyplot(Sepal.Length~Petal.Length|Species, data=iris, layout=c(1, 1))
print(p)
dev.off()

#-----------------------------------

animateTrellis(p, file='iris.svg')
 #---------------------------------