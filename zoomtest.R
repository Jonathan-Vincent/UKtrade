require(grDevices)
require(packcircles)
library("plotrix")
library(ggplot2)
library(cowplot)
library(ggrepel)

#pairs(~Exports99+Imports99+Imports19+Exports19+GDP99+GDP19,data=tradeGDP,
#      main="Simple Scatterplot Matrix")

traderatios = tradeGDP

traderatios[200,]$Country = 'USA'
traderatios[199,]$Country = 'UAE'
traderatios[69,]$Country = 'North Macedonia'
traderatios[25,]$Country = 'Bosnia'
#traderatios[172,]$Country = 'S. Africa'
#traderatios[19,]$Country = ''
traderatios[183,]$Country = ''
traderatios[178,]$Country = ''
traderatios[173,]$Country = ''
traderatios[53,]$Country = ''
traderatios[56,]$Country = ''
traderatios[191,]$Country = ''
traderatios[195,]$Country = ''
traderatios[44,]$Country = ''
traderatios[45,]$Country = ''
traderatios[110,]$Country = ''


traderatios$importgrowth = traderatios$Imports19/traderatios$Imports99
traderatios$exportgrowth = traderatios$Exports19/traderatios$Exports99
traderatios$tradegrowth = traderatios$Total19/(traderatios$Total99)
traderatios$gdpgrowth = traderatios$GDP19/traderatios$GDP99

traderatios$radius = 0.025
traderatios$radius[(traderatios$Total19)>100] = 0.05
traderatios$radius[(traderatios$Total19)>1000] = 0.1
traderatios$radius[(traderatios$Total19)>10000] = 0.2
traderatios$radius[(traderatios$Total19)>100000] = 0.4

showlimit = 1
traderatios = traderatios[traderatios$importgrowth<Inf,]
traderatios = traderatios[(traderatios$Total19)>showlimit,]

traderatios = na.omit(traderatios)
row.names(traderatios) <- 1:nrow(traderatios)

#pairs(~importgrowth+exportgrowth+gdpgrowth+tradegrowth,data=traderatios,
#      main="Simple Scatterplot Matrix")
#boundary = c(0.8,15)

#th = theme_classic()
#plot(traderatios$gdpgrowth,traderatios$tradegrowth,log='xy',xlim=boundary,ylim=boundary)
#reg2=lm(log(traderatios$tradegrowth,10)~log(traderatios$gdpgrowth,10),weights = traderatios$Total19)
#print(summary(reg2))
#reg3=lm(log(traderatios$tradegrowth,10)~log(traderatios$gdpgrowth,10))
#print(summary(reg3))
#reg4=lm(log(traderatios$tradegrowth,10)~log(traderatios$gdpgrowth,10),weights = traderatios$GDP19)
#print(summary(reg4))
#abline(reg2,col='blue')
#abline(reg4,col='red')
#abline(0,1)
#text(traderatios$gdpgrowth[traderatios$Total19>showlimit], traderatios$tradegrowth[traderatios$Total19>showlimit]*1.06, labels=traderatios$Country[traderatios$Total19>showlimit], cex= 0.7)

circleVerticesLL <- function(xc, yc, radius, logscale=2, npoints=100) {
  a <- seq(0, 2*pi, length.out = npoints + 1)
  x <- xc * logscale^(radius * cos(a))
  y <- yc * logscale^(radius * sin(a))
  m <- cbind("x" = x, "y" = y)
}
circleLayoutVerticesLL <- function(layout, logscale=2, npoints=100, xysizecols=1:3, 
                                   sizetype = c("radius", "area"),
                                   idcol=NULL) {
  sizetype <- match.arg(sizetype)
  layout <- as.data.frame(layout)
  xcol <- xysizecols[1]
  ycol <- xysizecols[2]
  sizecol <- xysizecols[3]
  
  if (is.null(idcol))
    ids <- 1:nrow(layout)
  else
    ids <- layout[[idcol]]
  
  if (sizetype == "area") layout[[sizecol]] <- sqrt(layout[[sizecol]] / pi) 
  
  verts <- lapply(
    1:nrow(layout), 
    function(i) {
      df <- as.data.frame(
        circleVerticesLL(layout[[i, xcol]], 
                         layout[[i, ycol]], 
                         layout[[i, sizecol]], 
                         logscale,
                         npoints) )
      
      df$id <- ids[i]
      df
    })
  
  do.call(rbind, verts)
}

circlemat = matrix(c(traderatios$gdpgrowth,traderatios$tradegrowth,traderatios$radius),  ncol = 3, nrow = nrow(traderatios))
circlematzoom = matrix(c(traderatios$gdpgrowth,traderatios$tradegrowth,traderatios$radius/1.75),  ncol = 3, nrow = nrow(traderatios))

dat.gg <- circleLayoutVerticesLL(circlemat, sizetype = "radius",npoints=200)
dat.ggzoom <- circleLayoutVertices(circlematzoom, sizetype = "radius",npoints=200)

bottomlim = 1
widthlim = 4.1
heightlim = widthlim

index = traderatios$gdpgrowth>widthlim|traderatios$tradegrowth>heightlim|traderatios$tradegrowth<bottomlim|traderatios$gdpgrowth<bottomlim
labs = traderatios
labs[labs$Total19>11777,]$Country = ' '

th = theme_classic()
breaks <- 2^(0:5)
minor_breaks <- (0:50)
p <- ggplot() +
  geom_abline(alpha = 0.3)+
  geom_rect(aes(xmin=bottomlim,ymin=bottomlim,xmax=widthlim,ymax=heightlim),fill='blue',alpha=0.2)+
  geom_polygon(data = dat.gg, aes(x, y, group = id),fill='orange', colour = alpha('black',alpha=0.5), alpha = 0.5) +
  geom_text_repel(data = labs[index,], aes(gdpgrowth, tradegrowth, label = Country),size=1.4,max.overlaps = 6,force=2,box.padding = 0.1,min.segment.length = 0.2)+
  geom_text(data = traderatios[index&traderatios$Total19>11777,], aes(gdpgrowth, tradegrowth, label = Country),size=1.4)+
  geom_text(aes(1.34,3.8,label='See Right'),size=1.8,col='white') + 
  scale_x_continuous(labels = c('1x','2x','4x','8x','16x','32x'),trans = 'log2',lim=c(0.8,20),breaks = breaks, minor_breaks = minor_breaks,expand = c(0,0)) +
  scale_y_continuous(labels = c('1x','2x','4x','8x','16x','32x'),trans = 'log2',lim=c(0.5,38),breaks = breaks, minor_breaks = minor_breaks,expand = c(0,0)) +
  xlab('Growth in GDP, 1999-2019')+
  ylab('Growth in trade with Britain, 1999-2019    ')+
  coord_equal()+
  theme_bw()

labs = traderatios
labs[labs$Total19>11777,]$Country = ''
index2 = labs$gdpgrowth>widthlim|labs$tradegrowth>heightlim|labs$tradegrowth<bottomlim|labs$gdpgrowth<bottomlim

legdata = data.frame(x1=c(-0.2,0.1,0.4,0.7,1.1), y1=rep(0,5), radius = c(0.025,0.05,0.1,0.2,0.4)/3,label= c('£0 bn','£0.1 bn','£1 bn','£10 bn','£100+ bn'))
circlematleg = matrix(c(legdata$x1,legdata$y1,legdata$radius),  ncol = 3, nrow = nrow(legdata))
dat.ggleg <- circleLayoutVertices(circlematleg, sizetype = "radius",npoints=200)

leg <- ggplot() +
  geom_polygon(data = dat.ggleg, aes(x, y, group = id), fill='orange',colour = alpha('black',alpha=0.5), alpha = 0.5) +  
  geom_text(data = legdata, size = 2,
            aes(x = x1, y = y1+0.17 , label = label))+
  geom_text(aes(-0.3,-0.1,label='Total Trade with Britain 2019:'),size=2,hjust=0)+
  theme_void()+
  coord_equal()+
  ylim(NA,0.2)+
  xlim(-0.3,1.25)+
  #scale_x_continuous(expand = c(0,0)) +
  #scale_y_continuous(expand = c(0,0))+
  theme(axis.title = element_blank())+
  theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  theme(legend.position = "none")
  
print(leg)

breaks <- (1:5)
q <- ggplot() + 
  geom_rect(aes(xmin=bottomlim,ymin=bottomlim,xmax=widthlim,ymax=heightlim),fill='blue',alpha=0.2)+
  geom_abline(alpha = 0.3)+
  geom_polygon(data = dat.ggzoom, aes(x, y, group = id), fill='orange',colour = alpha('black',alpha=0.5), alpha = 0.5) +  
  geom_text_repel(data = labs[!index2,], aes(gdpgrowth, tradegrowth, label = Country),size=1.4,max.overlaps = 5,force=2,box.padding = 0.13,min.segment.length = 0.3)+
  geom_text(data = traderatios[!index&traderatios$Total19>11777,], aes(gdpgrowth, tradegrowth, label = Country),size=1.4)+
  geom_text(aes(1.5,3.9,label='Zoom In'),size=4,col='white') + 
  scale_x_continuous(labels = c('1x','2x','3x','4x','5x'),lim=c(bottomlim,widthlim),breaks = breaks, minor_breaks = minor_breaks,expand = c(0,0)) +
  scale_y_continuous(labels = c('1x','2x','3x','4x','5x'),lim=c(bottomlim,heightlim),breaks = breaks, minor_breaks = minor_breaks,expand = c(0,0)) +
  coord_equal()+
  theme_bw()+
  theme(axis.title = element_blank())+
  theme(plot.margin = unit(c(0,0.1,0,0), "cm"))

rhs <- plot_grid(q,leg,ncol = 1,rel_heights = c(0.85,0.15))
r <- plot_grid(p,rhs)

save_plot('plots/r.png',r,base_height = 3,units= 'in')  
