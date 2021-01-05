require(grDevices)
require(packcircles)
library("plotrix")
library(ggplot2)
library(ggrepel)


trade40 = trade2040[trade2040$Total>10,]
trade40[1,'Country'] = 'European Union \nEU-27\n£939 bn'
trade40[2,'Country'] = 'United States\n£328 bn'
trade40[3,'Country'] = 'China \n£149 bn'
trade40[10,'Country'] = 'UAE'
trade40[43,'Country'] = 'Macedonia'
trade40[15,'Country'] = 'S. Arabia'
trade40[16,'Country'] = 'Korea'
trade40[17,'Country'] = 'South\nAfrica'
trade40[7,'Country'] = 'Hong\nKong'

trade40[30,'Country'] = '' #Kuwait
trade40[30,'Country'] = '' #Oman
trade40[33,'Country'] = '' #Pakistan
trade40[46,'Country'] = '' #Sri Lanka
trade40[48,'Country'] = '' #Oman

#Squish map, and move North africa/ME southwards
trade40[1,'Lat'] = trade40[1,'Lat']-5
trade40$Long[trade40$Long<(-140)] = trade40$Long[trade40$Long<(-140)]*0.7
trade40$Long[trade40$Long>140] = trade40$Long[trade40$Long>140]*0.83
trade40$Lat[trade40$Lat<40&trade40$Long<60&trade40$Long>-10] = trade40$Lat[trade40$Lat<40&trade40$Long<60&trade40$Long>-10]-20
trade40$Long[trade40$Long>35] = trade40$Long[trade40$Long>35]*1.2
trade40$Lat[trade40$Lat<0] = -(-trade40$Lat[trade40$Lat<0])^(0.6)


longs40 = as.numeric(unlist(trade40['Long']))/2
lats40 = as.numeric(unlist(trade40['Lat']))
totals40 = as.numeric(unlist(trade40['Total']))

circlemat40 = matrix(c(longs40,lats40,0.04*sqrt(totals40)),  ncol = 3, nrow = nrow(trade40))

wts40 <- rep(1.0, nrow(trade40))
wts40[ 1 ] <- 0.0

res40 <- circleRepelLayout(circlemat40, sizetype = "radius", weights=wts40)

dat.gg40 <- circleLayoutVertices(res40$layout, sizetype = "radius",npoints=200)
trade40 <- cbind(trade40, res40$layout)

fixed40 <- 18

labs40 <- data.frame(trade40[8:9])
labs40 <- rbind(labs40,dat.gg40[seq(1, 4001, 10),1:2])
#labs40 <- rbind(labs40,dat.gg40[seq(1001, 6001, 10),1:2])
#labs40 <- rbind(labs40,dat.gg40[seq(6001, 10001, 35),1:2])
labs40 <- rbind(labs40,dat.gg40[seq(4001, 10001, 40),1:2])
labs40 <- rbind(labs40,dat.gg40[seq(10001, nrow(dat.gg40), 100),1:2])
#add points to middle of circles to prevent covering up layers
labs40 <- rbind(labs40,labs40[1:fixed40,])
labs40 <- rbind(labs40,labs40[1:fixed40,])




labs40$lab = ''
labs40$lab[1:nrow(trade40)]= trade40$Country
labs40$lab[28]=''
labs40$lab[22]=''


exp40 <- 'Total Trade with Britain\nProjection\n£bn, 2019'

p3 <- ggplot() +
  geom_polygon(data = dat.gg40, aes(x, y, group = id), fill='orange', colour = alpha('black',alpha=0.5), alpha = 0.6) +
  #geom_text(data = labs40[nrow(trade40)+1:nrow(labs40),], aes(x, y, label = 'X')) + #Uncomment to show edges
  geom_text(aes(80,90,label='2040'),size=10) + 
  geom_text(aes(-78,88,label=exp40),size=4,hjust=0) + 
  geom_text_repel(data = labs40[c(fixed40+1:30,nrow(trade40)+1:nrow(labs40)),], aes(x, y, label = lab),size=2.85,force=3,max.overlaps =100) +
  geom_text(data = trade40[1:fixed40,], aes(x, y, label = Country),size=3) +
  coord_equal()+
  theme_void()+
  scale_x_continuous(expand = c(0, 0),limits = c(-80,100)) +
  scale_y_continuous(expand = c(0, 0),limits = c(-20,100))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
