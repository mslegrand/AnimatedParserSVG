library(grid)
library(gridSVG)
library(XML)
library(stringr)

draw.pak.svg<-function(pak.circle.id, face.color="gold", repeat.count="indefinite"){
  
  svg<-grid.export()
  #pak.node<-getNodeSet(svg$svg,"//svg:g[@id='pak-m.1']",  c(svg="http://www.w3.org/2000/svg"))[[1]]
 
    
  pak.circ.node<-getNodeSet(svg$svg, pak.circle.id,  c(svg="http://www.w3.org/2000/svg"))[[1]]
  
  circ.Attrs<-xmlAttrs(pak.circ.node)
  circ.radius<-as.numeric(circ.Attrs['r'])
  circ.cx<-(circ.Attrs['cx'])
  circ.cy<-(circ.Attrs['cy'])
  
  
  rad1<-as.character((circ.radius))
  rad2<-as.character((2*circ.radius))
  #heightX<-as.character((2*circ.radius))
  #try str_replace
  rad7<-as.character(.7*circ.radius)
  rad3<-as.character(.3*circ.radius)
  rad5<-as.character(.1*circ.radius)
   
  node.str<-
    "   <mask id=\"pac.mask\" >
<rect x=\"-rad1\" y=\"-rad2\" width=\"rad2\" height=\"rad2\" fill=\"white\" stroke=\"black\" >
<animateTransform attributeName=\"transform\" type=\"rotate\" begin=\"0s\" dur=\"1s\" values=\"0;-40;0\" repeatCount=\"repeat.count\"/>
</rect>
<rect x=\"-rad1\"  width=\"rad2\" height=\"rad2\" fill=\"white\" stroke=\"black\">
<animateTransform attributeName=\"transform\" type=\"rotate\" begin=\"0s\" dur=\"1s\" values=\"0;40;0\" repeatCount=\"repeat.count\"/>
</rect>
</mask>
<g transform=\"translate(circ.cx circ.cy)\">
<circle r=\"rad7\" fill=\"face.color\" mask=\"url(#pac.mask)\"/>
<circle cy=\"rad3\" r=\"rad5\" fill=\"#000\" stroke=\"white\" stroke-width=\"1\"/>
</g>
"
  
  node.str<-gsub("rad1",rad1,node.str,fixed=T)
  node.str<-gsub("rad2",rad2,node.str,fixed=T)
  node.str<-gsub("rad7",rad7,node.str,fixed=T)
  node.str<-gsub("rad3",rad3,node.str,fixed=T)
  node.str<-gsub("rad5",rad5,node.str,fixed=T)
  node.str<-gsub("circ.cx",circ.cx,node.str,fixed=T)
  node.str<-gsub("circ.cy",circ.cy,node.str,fixed=T)
  node.str<-gsub("repeat.count",repeat.count,node.str,fixed=T)
  node.str<-gsub("face.color",face.color,node.str,fixed=T)
  
  #node.str<-gsub("heightX",heightX,node.str,fixed=T)
  
  
  txt<-saveXML(svg$svg, indent=T)
  tmp<-saveXML(pak.circ.node, indent=T)
  #txt2<-str_replace(txt, tmp, node.str)
  txt2<-sub(tmp,node.str,txt, fixed=T)
  temp<-xmlParse(txt2, asText=T)
  temp
}


# node.str<-str_replace(mode.str,100,rad1)
# node.str<-str_replace(mode.str,200,rad2)
# 
#node.xml<-xmlTreeParse(node.str, asText=TRUE)

#addNode(node.str, pak.circ.node, svg$svg)
#addNode(node, parent, to, ...)
test.pak.animate<-function(){
  plot.new()
  grid.newpage()
  grid.rect(width=1,height=1)
  r<-.04    
  cirg<-circleGrob(x=0.5, y=0.5, r=r, name="circ", gp=gpar(fill="yellow"))
  pak<-gTree(children=gList(cirg), name="pak-m")   
  grid.draw(pak)
  
  pak.circle.id<-"//svg:circle[@id='circ.1.1']"
  temp<-draw.pak.svg(pak.circle.id, "red", "3")
  saveXML(temp, "tmpPak.svg")
  
}

#test.pak.animate()


