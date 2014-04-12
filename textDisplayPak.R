source("pegSVGWidgets.R")


if(!exists("pegR")){
  source("parseExp.R") 
}

draw.new<-function(){
  plot.new()
  grid.newpage()
  vpTop<-viewport(.5, 1, width=1, height=5/6, just="top", clip="on", name="upper")
  pushViewport(vpTop)
  upViewport(0)
  vpBot<-viewport(.5, 0, width=1, height=1/6, just="bottom", clip="on", name="lower")
  pushViewport(vpBot)
  vpTxt<-viewport(x=.5, y=.5, width=1, height=.3, name="text", clip="on")
  pushViewport(vpTxt)
  upViewport(0)
  seekViewport("lower")
  vpPac<-viewport(x=.5, y=.5, width=.06, height=.5, name="pac")
  pushViewport(vpPac)
  #grid.rect(width=1, height=1, gp=gpar(fill="red"))
  #grid.rect(gp=gpar(fill="red"),vp=vpBot)  
  upViewport(0)
}


g.pak<-function(){
  head<-circleGrob(x=.5,y=.5, r=.5, gp=gpar(fill="yellow"), name="pac-head")
}


# g.pak<-function( bk.grnd="lightblue3"){ #lightblue3
#   seekViewport("text")
#   #convert height into some kind of fixed units
#   #grid.rect(.5,.5,.5,.5, gp=gpar(fill="black"))
#   h<-1.5
#   hin<-convertHeight(unit(h,"npc"), "in")
#   wh<-convertWidth(hin, "npc")
#   #vpc<-viewport(wh, .5, wh,  2,  clip =TRUE)
#   vpc<-viewport(.5, .5, wh,  2,  clip =TRUE)
#   #vpc<-viewport(wh, .5, wh,  h,  clip =TRUE)
#   #pushViewport(vpc)
#   #grid.rect(.5,.5,1,1, gp=gpar(col="red"))
#   vppac<-viewport(.5,.5, 1.5, 1.5, name="pac", angle=45)
#   #pushViewport(vppac)
#   #grid.rect(.5, .5, 1.5,  1.5, gp=gpar(col="red"))
#   #   head<-grid.circle(x=.5,y=.5, r=.5, gp=gpar(fill="yellow"), name="pac-head")
#   #   mouth<-grid.rect(x=1, y=0, width=1, height=1, ,gp=gpar(fill=bk.grnd, col=bk.grnd), name="pac-mouth")  
#   head<-circleGrob(x=.5,y=.5, r=.5, gp=gpar(fill="yellow"), name="pac-head")
#   #mouth<-rectGrob(x=1, y=0, width=1, height=1, ,gp=gpar(fill=bk.grnd, col=bk.grnd), name="pac-mouth",vp=vppac)  
#   #mouth<-grid.rect(x=.5, y=.5, width=.5, height=.5, ,gp=gpar(fill=bk.grnd, col=bk.grnd), name="pac-mouth") 
#   #vp<-vpStack()
#   #pt<-gTree(children=gList(head, mouth), name="pac-m", vp=vpc) 
#   pt<-gTree(children=gList(head), name="pac-m", vp=vpc) 
#   #grid.draw(pt)
#   upViewport(0)
#   #seekViewport("text")
#   #grid.draw(pt)
#   #seekViewport("pac")
#   #grid.draw(pt)
#   pt
# }
# 

# id.new.TEXT<-counter.new("TXT-")
text.pos<-1
wh<-0




draw.TextPanel<-function( text.input, pos){
  seekViewport("text")
  grid.rect(width=1,height=.5, gp=gpar(fill="lightgreen"))
  xa<-strsplit(text.input,"")[[1]]
  
#  h<-2 #1.5
#  hin<-convertHeight(unit(h,"npc"), "in")
#  wh<<-convertWidth(hin, "npc")  
  #w<-grobWidth(textGrob('MM'))
  
  gtr<-gTree(name="node.TEXT", vp=viewport(x=.5, just="left"))
  for(i in 1:length(xa)){
    #x<-.5+(.05)*(2*i-1)
    x<-(.05)*(2*i-1)
    alpha=1 
    tg<-textGrob(xa[i] , x=x, y=.5, name=paste0("txt-",i),  gp=gpar(col="black", alpha=alpha ))
    cg<-circleGrob(x=x, y=.5, r=.3, name=paste0("cgck-",i), gp=gpar(fill="white", alpha=alpha, col="white") )
    bg<-gTree(name=paste0("lg-",i), children=gList(cg,tg))
    gtr<-addGrob(gtr, bg )
  }
  grid.draw(gtr)
  
  pak<-g.pak()
  #seekViewport("lower")
  upViewport(0)
  seekViewport("pac")
  grid.draw(pak)
  text.pos<<-1  
}

# draw.TextPanel<-function( text.input, pos){
#   seekViewport("text")
#   xa<-strsplit(text.input,"")[[1]]
#   
#   h<-1.5
#   hin<-convertHeight(unit(h,"npc"), "in")
#   wh<<-convertWidth(hin, "npc")  
#   w<-grobWidth(textGrob('MM'))
#   
#   gtr<-gTree(name="node.TEXT", vp=viewport( x=wh, just="left"))  
#   for(i in 1:length(xa)){
#     #x<-unit(.5,"npc") + wh*.25 -.5*w + 1.5*(i)*w # convertWidth(unit(.5, "snpc")) + (i+1)*w
#     #x<-unit(.5,"npc") + wh*.5 -.75*w + 1.5*(i)*w # convertWidth(unit(.5, "snpc")) + (i+1)*w
#     x<-  1.0*(i)*wh   # convertWidth(unit(.5, "snpc")) + (i+1)*w
#     alpha=1 #ifelse(i<9, 1, .5)
#     tg<-textGrob(xa[i] , x=x, name=paste0("txt-",i),  gp=gpar(col="black", alpha=alpha ))
#     rg<-rectGrob(x=x, y=.5, width=.5*wh, height=1, name=paste0("bck-",i), gp=gpar(fill="white", alpha=alpha, col="white") )
#     #bg<-gTree(name=paste0("lg-",i), children=gList(rg,tg))
#     #gtr<-addGrob(gtr, bg )
#     gtr<-addGrob(gtr, tg )
#   }
#   #gtr<-grid.edit("node.TEXT", vp=viewport(x=.5, just="left"))
#   #gPath("node.TEXT",names[i]), vp=vpt)
#   grid.draw(gtr)
#   
#   pak<-g.pak()
#   #seekViewport("lower")
#   seekViewport("pac")
#   grid.draw(pak)
#   text.pos<<-1  
# }
# 



text.pos.right<-function(count=1){
  seekViewport("text")
  next.pos<-text.pos+count
  length(childNames(grid.get(gPath("node.TEXT"))))->text.len
  if(next.pos<=text.len+1){
    #  x<-(.05)*(2*i-1) =>
    #  if text.pos==1 then cx==.5
    #  if text.pos==2 then cx==.5-(2*.05)
      
    while(text.pos<next.pos){
      gtr<-grid.edit("node.TEXT", vp=viewport(x=.5-(text.pos)*(2*.05), just="left"))
      grid.edit(gPath("node.TEXT" ,paste0("lg-",text.pos)),  gp=gpar(alpha=.4)) #or lg-
      text.pos<-text.pos+1
    }    
    text.pos<<-next.pos    
  }
}

text.pos.left<-function(count=1){
  seekViewport("text")
  next.pos<-text.pos-count  
  if( 0 < next.pos ){     
    while(text.pos>next.pos){
      text.pos<-text.pos-1
      gtr<-grid.edit("node.TEXT", vp=viewport(x=.5-(text.pos-1)*(2*.05), just="left"))
      grid.edit(gPath("node.TEXT" ,paste0("lg-",text.pos)),  gp=gpar(alpha=1))      
#       grid.edit(gPath("node.TEXT" ,paste0("txt-",text.pos)),  gp=gpar(alpha=1))      
    }
    text.pos<<-next.pos
  } 
}

text.pos.eat.ani<-function(count=1){
  seekViewport("text")
  next.pos<-text.pos+count 
  length(childNames(grid.get(gPath("node.TEXT"))))->text.len
  x1=(text.pos-.5)*(2*.05)
  x2=(next.pos-.5)*(2*.05)
  #x1=0.1
  beginning<-0
  cat("textPos=",text.pos,"nextPos=",next.pos,"\n")
  cat("x1=",x1,"x2=",x2,"\n")
  for(i in 1:text.len){
    x2<-(.5-text.pos+i)*(2*.05)
    x1<-(.5-next.pos+i)*(2*.05)
    x0<-(x1+x2)/2
    grid.animate(gPath("node.TEXT" ,paste0("lg-",i), paste0("cgck-",i)), x= c(x2,x1), 
                 begin=beginning,
                 duration=2, rep=F,  redraw=T)
    grid.animate(gPath("node.TEXT" ,paste0("lg-",i), paste0("txt-",i)), x= c(x2,x1), 
                 begin=beginning,
                 duration=2, rep=F,  redraw=T) 
    #turn the text.pos to .2  
  }
  i<-text.pos
  grid.animate(gPath("node.TEXT" ,paste0("lg-",i), paste0("cgck-",i)), "fill-opacity"=c(1,.4), 
                  begin=beginning+1,
                  rep=F,  redraw=T)
  grid.animate(gPath("node.TEXT" ,paste0("lg-",i), paste0("txt-",i)), "opacity"=c(1,.2), 
               begin=beginning+1,
               duration=2, rep=F,  redraw=T)
  
  
  
#   x2<-(.5-text.pos+i)*(2*.05)
#   x1<-(.5-next.pos+i)*(2*.05)
  #animateGrob(grid.get(gPath("node.TEXT")), x= c(.1,.9), duration=5, rep=T, group=T, redraw=T)
  #grid.animate(grid.get(gPath("node.TEXT")), x= c(.1,.9), duration=5, rep=T, group=T, redraw=T)
  #grid.animate(gPath("node.TEXT" ,paste0("lg-",2)), x= c(.1,.9), duration=2, rep=T, group=T, redraw=T)
#    grid.animate(gPath("node.TEXT" ,paste0("lg-",i), paste0("cgck-",i)), x= c(x2,x1), duration=2, rep=T,  redraw=T)
#    grid.animate(gPath("node.TEXT" ,paste0("lg-",i), paste0("txt-",i)), x= c(x2,x1), duration=2, rep=T,  redraw=T)
#   grid.animate(gPath("node.TEXT" ,paste0("lg-",2), "cgck-2"), x= c(x2,x1), duration=2, rep=T,  redraw=T)
#  grid.animate(gPath("node.TEXT" ,paste0("lg-",2)), x= c(x2,x1), duration=2, rep=T,  redraw=T)
  #grid.edit(gPath("node.TEXT" ,paste0("lg-",2), "cgck-2"),gp=gpar(fill="white"))
  
  #   if( 0 < next.pos ){     
#     while(text.pos>next.pos){
#       text.pos<-text.pos-1
#       gtr<-grid.edit("node.TEXT", vp=viewport(x=.5-(text.pos-1)*(2*.05), just="left"))
#       grid.edit(gPath("node.TEXT" ,paste0("lg-",text.pos)),  gp=gpar(alpha=1))      
#       #       grid.edit(gPath("node.TEXT" ,paste0("txt-",text.pos)),  gp=gpar(alpha=1))      
#     }
#     text.pos<<-next.pos
#   } 
}

# 
text.pos.back<-function(next.pos){
  seekViewport("text")
  if( 0 < next.pos){ #the next pos is x+pos     
    while(text.pos>next.pos){
      text.pos<-text.pos-1
      gtr<-grid.edit("node.TEXT", vp=viewport(x=.5-(text.pos-1)*(2*.05), just="left"))
      grid.edit(gPath("node.TEXT" ,paste0("txt-",text.pos)),  gp=gpar(alpha=1))      
      #       grid.edit(gPath("node.TEXT" ,paste0("txt-",text.pos)),  gp=gpar(alpha=1))      
    }
    text.pos<<-next.pos
  } 
}


#------------------------------------------


mark.stack<-c()

new.mark<-function(level, id, color="blue"){
#   #start a line
#   fname=paste0("FR")
#   tname=paste0("TO")
#   #gTree=
#   seekViewport("text")
#   x<-(1 + text.pos)*wh  
#   y<--.5 -(.3*level) # unit(-.6,"npc") + (-level) * wh
#   tid<-id
#   mvto<-moveToGrob(x = x - .2*wh, y = y, , name = fname) 
#   lnto<-lineToGrob(x = x + .2*wh, y = y , name=tname)
#   gt<-gTree(name=tid, children=gList(mvto, lnto), gp=gpar(col=color,lwd=3))
#   grid.draw(gt)
#   gt
}


id.new.mark<-  counter.new("MARK-")

drop.mark<-function(){
#   level<-length(mark.stack)+1
#   col.choice<-c("red", "blue", "yellow", "darkmagenta", "darkgreen")
#   cur.color<-col.choice[( (level-1) %% length(col.choice))+1]
#   mark.id<-id.new.mark()
#   new.mark(level,mark.id, cur.color)
#   #push onto mark.stack
#   mark.stack<<-c(mark.id, mark.stack)  
}


pop.mark<-function(){
#   mark.id<-mark.stack[1]
#   mark.stack<<-mark.stack[-1]
#   grid.remove(gPath(mark.id))
}

update.marks<-function(){
#   x<-(1.2 + text.pos)*wh  
#   for(id in mark.stack){
#     grid.edit(gPath(id,"TO"), x=x)
#   }
}





test.Pak<-function(){
  library(gridSVG)
  #print("hi")
  #----test code---------
  #value(pegR[["GSEQ"]]("'x'/ 'y'"))->res
  #value(pegR[["GSEQ"]]('! x '))->res
  value(pegR[["GSEQ"]]("'a' / ( 'b' ('e'/ 'c') ) 'd' 'd' (x/y) !('a' b / & 'c')'"))->res
  
  
  
  #value(pegR[["GSEQ"]]('!(a /! b)'))->res
  # value(pegR[["GSEQ"]]("  'k' / 'b' " ))->res
  # value(pegR[["GSEQ"]]("  A / B " ))->res
  # 
  # value(pegR[["GSEQ"]]("  K / 'b' " ))->res
  # value(pegR[["GSEQ"]]("  'k' / B " ))->res
  #value(pegR[["GSEQ"]]("  'k' / 'b' " ))->res
  
  # value(pegR[["GSEQ"]]("  'k'* " ))->res
  #m
  
  draw.new()
  tree<-build.tree(res)
  drawPegTree(tree)
  
  drawLower()
  
 text.input<-"shillo there way, I hope it is all good!"
  #text.input<-"he"
  draw.TextPanel(text.input)
  
  
#   for(i in 1:1){
#     text.pos.right()
#     Sys.sleep(1)
#   }
#   

  text.pos.eat.ani()
  grid.export("zzz.svg")
  
#   svg<-grid.export()  
#   library(XML)
#   svgdoc <- svg$svg
#   pacNode <- getNodeSet(svgdoc, "//svg:g[@id='pac-m.1']", c(svg="http://www.w3.org/2000/svg"))[[1]]
                           
  
  #   library(selectr)
  #   css_to_xpath("#pac-m")
  #   
  
#   tmp<-readline("press to continue
#                 ")
#   for(i in 1:10){
#     text.pos.right()
#     Sys.sleep(1)
#   }
}

test.Pak()