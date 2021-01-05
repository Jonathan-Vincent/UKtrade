require(grDevices)
require(packcircles)
library("plotrix")
library(ggplot2)
library(ggrepel)


trade99 = trade1999EU[trade2040$Total>10,]
trade99[1,'Country'] = 'European Union \nEU-27\n£284 bn'
trade99[2,'Country'] = 'United States \n£79 bn'
trade99[19,'Country'] = 'UAE'
trade99[13,'Country'] = 'S. Arabia'
trade99[14,'Country'] = 'Korea'

#Squish map, and move North africa/ME southwards

trade99[1,'Lat'] = trade99[1,'Lat']-5
trade99$Long[trade99$Long<(-140)] = trade99$Long[trade99$Long<(-140)]*0.7
trade99$Long[trade99$Long>140] = trade99$Long[trade99$Long>140]*0.83
trade99$Lat[trade99$Lat<40&trade99$Long<60&trade99$Long>-10] = trade99$Lat[trade99$Lat<40&trade99$Long<60&trade99$Long>-10]-20
trade99$Long[trade99$Long>35] = trade99$Long[trade99$Long>35]*1.2
trade99$Lat[trade99$Lat<0] = -(-trade99$Lat[trade99$Lat<0])^(0.6)
#trade99$Long[trade99$Long<0] = -(-trade99$Long[trade99$Long<0])^(0.95)

longs99 = as.numeric(unlist(trade99['Long']))/2
lats99 = as.numeric(unlist(trade99['Lat']))
totals99 = as.numeric(unlist(trade99['Total']))

circlemat99 = matrix(c(longs99,lats99,0.04*sqrt(totals99)),  ncol = 3, nrow = nrow(trade99))

wts99 <- rep(1.0, nrow(trade99))
wts99[ 1:3 ] <- 0.0

res99 <- circleRepelLayout(circlemat99, sizetype = "radius", weights=wts99)

dat.gg99 <- circleLayoutVertices(res99$layout, sizetype = "radius",npoints=200)
trade99 <- cbind(trade99, res99$layout)

fixed99 <- 3

labs99 <- data.frame(trade99[8:9])
labs99 <- rbind(labs99,dat.gg99[seq(1, 6001, 10),1:2])
#labs99 <- rbind(labs99,dat.gg99[seq(6001, 10001, 35),1:2])
labs99 <- rbind(labs99,dat.gg99[seq(6001, 12001, 50),1:2])
#add points to middle of circles to prevent covering up layers
labs99 <- rbind(labs99,labs99[1:fixed99,])


labs99$lab = ''
labs99$lab[1:nrow(trade99)]= trade99$Country

exp99 <- 'Total Trade with Britain\n£bn, 1999'

p1 <- ggplot() +
  geom_polygon(data = dat.gg99, aes(x, y, group = id), fill='orange',  colour = alpha('black',alpha=0.5), alpha = 0.6) +
  #geom_text(data = labs99[nrow(trade99)+1:nrow(labs99),], aes(x, y, label = 'X')) + #Uncomment to show edges
  geom_text(aes(80,90,label='1999'),size=10) + 
  geom_text(aes(-78,92,label=exp99),size=4,hjust=0) + 
  geom_text_repel(data = labs99[c(fixed99+1:35,nrow(trade99)+1:nrow(labs99)),], aes(x, y, label = lab),size=2.85,force=1,max.overlaps =150) +
  geom_text(data = trade99[1:fixed99,], aes(x, y, label = Country),size=3) +
  coord_equal()+
  theme_void()+
  scale_x_continuous(expand = c(0, 0),limits = c(-80,100)) +
  scale_y_continuous(expand = c(0, 0),limits = c(-20,100))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
