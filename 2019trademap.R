require(grDevices)
require(packcircles)
library("plotrix")
library(ggplot2)
library(ggrepel)


trade19 = tradesorted[trade2040$Total>10,]
trade19[1,'Country Name'] = 'European Union \nEU-27\n£668 bn'
trade19[2,'Country Name'] = 'United States \n£232 bn'
trade19[3,'Country Name'] = 'China \n£80 bn'
trade19[10,'Country Name'] = 'UAE'
trade19[43,'Country Name'] = 'Macedonia'
trade19[15,'Country Name'] = 'S. Arabia'
trade19[16,'Country Name'] = 'Korea'
trade19[17,'Country Name'] = 'South\nAfrica'
trade19[7,'Country Name'] = 'Hong\nKong'
trade19[18,'Country Name'] = '' #Qatar

#Squish map move North africa/ME southwards

trade19[1,'Lat'] = trade19[1,'Lat']-5
trade19$Long[trade19$Long<(-140)] = trade19$Long[trade19$Long<(-140)]*0.7
trade19$Long[trade19$Long>140] = trade19$Long[trade19$Long>140]*0.83
trade19$Lat[trade19$Lat<40&trade19$Long<60&trade19$Long>-10] = trade19$Lat[trade19$Lat<40&trade19$Long<60&trade19$Long>-10]-20
trade19$Long[trade19$Long>35] = trade19$Long[trade19$Long>35]*1.2
trade19$Lat[trade19$Lat<0] = -(-trade19$Lat[trade19$Lat<0])^(0.6)


longs19 = as.numeric(unlist(trade19['Long']))/2
lats19 = as.numeric(unlist(trade19['Lat']))
totals19 = as.numeric(unlist(trade19['Total']))

circlemat19 = matrix(c(longs19,lats19,0.04*sqrt(totals19)),  ncol = 3, nrow = nrow(trade19))

wts19 <- rep(1.0, nrow(trade19))
wts19[ 0:3 ] <- 0.0

res19 <- circleRepelLayout(circlemat19, sizetype = "radius", weights=wts19)

dat.gg19 <- circleLayoutVertices(res19$layout, sizetype = "radius",npoints=200)
trade19 <- cbind(trade19, res19$layout)

fixed19 <- 10

labs19 <- data.frame(trade19[8:9])
labs19 <- rbind(labs19,dat.gg19[seq(1, 601, 2),1:2])
labs19 <- rbind(labs19,dat.gg19[seq(601, 6001, 10),1:2])
labs19 <- rbind(labs19,dat.gg19[seq(6001, 12001, 40),1:2])
#add points to middle of circles to prevent covering up layers
labs19 <- rbind(labs19,labs19[1:fixed19,])
labs19 <- rbind(labs19,labs19[1:fixed19,])
labs19 <- rbind(labs19,labs19[1:fixed19,])
labs19 <- rbind(labs19,labs19[1:fixed19,])
labs19 <- rbind(labs19,labs19[1:fixed19,])



labs19$lab = ''
labs19$lab[1:nrow(trade19)]= trade19$`Country Name`
#Thailand and Bangladesh don't fit
labs19$lab[28]=''
labs19$lab[22]=''


exp19 <- 'Total Trade with Britain\n£bn, 2019'

p2 <- ggplot() +
  geom_polygon(data = dat.gg19, aes(x, y, group = id), fill='orange',  colour = alpha('black',alpha=0.5), alpha = 0.6) +
  #geom_text(data = labs19[nrow(trade19)+1:nrow(labs19),], aes(x, y, label = 'X')) + #Uncomment to show edges
  geom_text(aes(80,90,label='2019'),size=10) + 
  geom_text(aes(-78,92,label=exp19),size=4,hjust=0) + 
  geom_text_repel(data = labs19[c(fixed19+1:35,nrow(trade19)+1:nrow(labs19)),], aes(x, y, label = lab),size=2.85,force=3,max.overlaps =150) +
  geom_text(data = trade19[1:fixed19,], aes(x, y, label = `Country Name` ),size=3) +
  coord_equal()+
  theme_void()+
  scale_x_continuous(expand = c(0, 0),limits = c(-80,100)) +
  scale_y_continuous(expand = c(0, 0),limits = c(-20,100))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
