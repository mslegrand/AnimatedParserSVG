library(grid)
library(gridSVG)

draw.new()
rg<-rectGrob(width=1,height=1, name="bg") 
grid.draw(rg)
fill2 <- radialGradient(c(rgb(1,0,0,.2), rgb(1,1,0,.4)),
                        stops=c(0, .7))
fill3 <- linearGradient(col = c("red", "white","blue"),
                        stops = c(0,  .25,  1),
                        spreadMethod="reflect")
                        #gradientUnits = c("bbox", "coords"),
#                         x0 = unit(0, "npc"), x1 = unit(1, "npc"),
#                         y0 = unit(0, "npc"), y1 = unit(1, "npc"),
#                         default.units = "npc",
#                         spreadMethod = c("pad", "reflect", "repeat"))

grid.gradientFill("bg", fill3)

grid.export(name="temp.svg")

primToDev.boxedtext <- function(x, dev) {  primToDev(boxedtext(x), dev) }


#XXX1. clipping of pak- get rid of <===== x
# 2. pak off centre?? (draw pac in svg)
# 3. Text on buttons- fix? <=====
# 4. add classes for our gtree objects?
# 5. animated motion? 
# 6. gradient fill?
# 7. success of failure notification
# 8. What to do at step
#     A. on step save to file?
#     B. save only changes? (and use d3 javascript to updata)
#     B. add pause and update changes (for animation)
#     C. sleep and wait for call from browser (via shiny)