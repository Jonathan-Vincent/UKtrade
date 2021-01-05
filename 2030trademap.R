require(grDevices)
require(packcircles)
library("plotrix")
library(ggplot2)
library(ggrepel)


trade = trade2030[trade2030$Total>100,]
trade[1,'Lat'] = trade[1,'Lat']-5
trade[19,'Country'] = 'UAE'
trade[35,'Long'] = trade[35,'Long']-30

#Squish map, and move North africa/ME southwards

trade$Lat[trade$Lat<40&trade$Long<60&trade$Long>-10] = trade$Lat[trade$Lat<40&trade$Long<60&trade$Long>-10]-20
trade$Long[trade$Long>35] = trade$Long[trade$Long>35]*1.2
trade$Lat[trade$Lat<0] = -(-trade$Lat[trade$Lat<0])^(0.6)
#trade$Long[trade$Long<0] = -(-trade$Long[trade$Long<0])^(0.95)


longs = as.numeric(unlist(trade['Long']))/2
lats = as.numeric(unlist(trade['Lat']))
totals = as.numeric(unlist(trade['Total']))
codes = as.character(unlist(trade['Code']))
names = as.character(unlist(trade['Country']))

circlemat = matrix(c(longs,lats,0.04*sqrt(totals)),  ncol = 3, nrow = nrow(trade))

largest.ids <- which.max(circlemat[,3])
wts <- rep(1.0, nrow(trade))
wts[ 0:3 ] <- 0.0

res <- circleRepelLayout(circlemat, sizetype = "radius", weights=wts)

dat.gg <- circleLayoutVertices(res$layout, sizetype = "radius",npoints=200)
trade <- cbind(trade, res$layout)

fixed <- 12

labs <- data.frame(trade[8:9])
labs <- rbind(labs,dat.gg[seq(1, 1001, 4),1:2])
labs <- rbind(labs,dat.gg[seq(1001, 6001, 10),1:2])
#labs <- rbind(labs,dat.gg[seq(6001, 10001, 35),1:2])
labs <- rbind(labs,dat.gg[seq(6001, nrow(dat.gg), 50),1:2])
#add points to middle of circles to prevent covering up layers
labs <- rbind(labs,labs[1:fixed,])



labs$lab = ''
labs$lab[1:nrow(trade)]= trade$Country
labs$lab[28]=''
labs$lab[22]=''


th = theme_classic()
head(dat.gg)
p <- ggplot() +
  geom_polygon(data = dat.gg, aes(x, y, group = id), fill='orange', colour = "black", alpha = 0.6) +
  #geom_text(data = labs[nrow(trade)+1:nrow(labs),], aes(x, y, label = 'X')) + #Uncomment to show edges
  geom_text_repel(data = labs[c(fixed+1:35,nrow(trade)+1:nrow(labs)),], aes(x, y, label = lab),size=2.85,force=4,max.overlaps =60) +
  geom_text(data = trade[1:fixed,], aes(x, y, label = names[1:fixed]),size=3) +
  coord_equal()+
  theme_void()
print(p)