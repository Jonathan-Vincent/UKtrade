require(grDevices)
require(packcircles)
library("plotrix")
library(ggplot2)

#pairs(~Exports99+Imports99+Imports19+Exports19+GDP99+GDP19,data=tradeGDP,
#      main="Simple Scatterplot Matrix")
par(bg="white")
traderatios = tradeGDP
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

#pairs(~importgrowth+exportgrowth+gdpgrowth+tradegrowth,data=traderatios,
#      main="Simple Scatterplot Matrix")
boundary = c(0.8,15)

th = theme_classic()
plot(traderatios$gdpgrowth,traderatios$tradegrowth,log='xy',xlim=boundary,ylim=boundary)
reg2=lm(log(traderatios$tradegrowth,10)~log(traderatios$gdpgrowth,10),weights = traderatios$Total19)
#print(summary(reg2))
reg3=lm(log(traderatios$tradegrowth,10)~log(traderatios$gdpgrowth,10))
#print(summary(reg3))
reg4=lm(log(traderatios$tradegrowth,10)~log(traderatios$gdpgrowth,10),weights = traderatios$GDP19)
print(summary(reg4))
#abline(reg2,col='blue')
abline(reg4,col='red')
abline(0,1)
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

dat.gg <- circleLayoutVerticesLL(circlemat, sizetype = "radius",npoints=100)

exptext1 <- 'Trade between the UK and Luxembourg'
exptext2 <- 'grew 11-fold between 1999 and 2019,'
exptext3 <- 'despite Luxembourg\'s GDP only'
exptext4 <- 'growing 2-fold in that time'


th = theme_classic()
breaks <- 2^(0:5)
minor_breaks <- (0:50)
p <- ggplot() +
  geom_abline(alpha = 0.3)+
  geom_rect(aes(xmin=1,ymin=1,xmax=5,ymax=5),fill='blue',alpha=0.2)+
  geom_polygon(data = dat.gg, aes(x, y, group = id),fill='orange', colour = "black", alpha = 0.7) +
  geom_text(data = traderatios, aes(gdpgrowth, tradegrowth, label = Code,size=radius)) +
  #geom_text(data = trade[21:nrow(trade),], aes(x-radius-2.7, y, label = Code)) +
  scale_x_continuous(trans = 'log2',lim=c(0.8,32),breaks = breaks, minor_breaks = minor_breaks,expand = c(0,0)) +
  scale_y_continuous(trans = 'log2',lim=c(0.7,32),breaks = breaks, minor_breaks = minor_breaks,expand = c(0,0)) +
  xlab('Growth in GDP, 1999-2019')+
  ylab('Growth in trade with Britain, 1999-2019')+
  coord_equal()+
  theme_bw()+
  theme(legend.position="none")
print(p)
