# x, y: data

# slices: number of horizontal slices through the data
# lboost: coefficient to increase the height of the lines
# gboost: coefficient to increase the height of the graph (ylim)
# xinc: horizontal offset for each succesive slice
# (typically something like 1/80)
# yinc: vertical offset for each succesive slice
# bcol: background color
# fcol: fill color for each slice (polygon)
# lcol: line color for each slice
# lwidth: line width
# NB if you want to cycle slice colors through vectors, you
# need to change the function code; it sounds like a
# pretty bad idea to me, but each to their own.

slicedens<-function(x,y,slices=50,lboost=1,gboost=1,xinc=0,yinc=0.01,
                    bcol="black",fcol="black",lcol="white",lwidth=2) {
  ycut<-min(y)+((0:(slices))*(max(y)-min(y))/slices)
  height<-gboost*((slices*yinc)+max(density(x)$y))
  plot( c(min(x),max(x)+((max(x)-min(x))/4)),
        c(0,height),
        xaxt="n",yaxt="n",ylab="",xlab="")
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=bcol)
  for(i in slices:1) {
    miny<-ycut[i]
    maxy<-ycut[i+1]
    gx<-(i-1)*(max(x)-min(x))*xinc
    gy<-(i-1)*(height)*yinc
    dd<-density(x[y>=miny & y<maxy])
    polygon(dd$x+gx,lboost*dd$y+gy,col=fcol)
    lines(dd$x+gx,lboost*dd$y+gy,col=lcol,lwd=lwidth)
  }
}

# Example:
y<-runif(5000,min=-1,max=1)
x<-runif(5000,min=-1,max=1)+rnorm(5000,mean=1/(y+1.1),sd=0.5-(y*0.5))
slicedens(x,y,lboost=0.2,fcol=rgb(0,0,0,200,maxColorValue=255))