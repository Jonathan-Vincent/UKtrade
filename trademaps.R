require(grDevices)
require(packcircles)
library("plotrix")
library(ggplot2)
library(ggrepel)

t99 = trade1999EU
t19 = tradesorted
t40 = trade2040

t40$cat = 'Other'
t40[1,]$cat='EU'
t40[2,]$cat='US'
t40[t40$Region %in% c('EA','SEA','CA','SA','MENA'),]$cat='Asia'

t99new = merge(t99[1],t40[c(1,8)])
t99new = merge(t99new,t99[c(1,5)])


bardata <- data.frame(name=c('EU','US','Asia','Other'),
                      t40=c(sum(t40$Total[t40$cat=='EU']),
                            sum(t40$Total[t40$cat=='US']),
                            sum(t40$Total[t40$cat=='Asia']),
                            sum(t40$Total[t40$cat=='Other'])),
                      t19=c(sum(t40$Previous[t40$cat=='EU']),
                            sum(t40$Previous[t40$cat=='US']),
                            sum(t40$Previous[t40$cat=='Asia']),
                            sum(t40$Previous[t40$cat=='Other'])),
                      t99=c(sum(t99new$Total[t99new$cat=='EU']),
                           sum(t99new$Total[t99new$cat=='US']),
                           sum(t99new$Total[t99new$cat=='Asia']),
                           sum(t99new$Total[t99new$cat=='Other'])))


bardata <- data.frame(name=c('1999','2019','2040'),
                      EU=c(sum(t99new$Total[t99new$cat=='EU']),
                            sum(t40$Previous[t40$cat=='EU']),
                            sum(t40$Total[t40$cat=='EU'])),
                      
                      US=c(sum(t99new$Total[t99new$cat=='US']),
                           sum(t40$Previous[t40$cat=='US']),
                           sum(t40$Total[t40$cat=='US'])),
                      
                      Asia=c(sum(t99new$Total[t99new$cat=='Asia']),
                           sum(t40$Previous[t40$cat=='Asia']),
                           sum(t40$Total[t40$cat=='Asia'])),
                      
                      Other=c(sum(t99new$Total[t99new$cat=='Other']),
                           sum(t40$Previous[t40$cat=='Other']),
                           sum(t40$Total[t40$cat=='Other'])))

bardata <- data.frame(year=c(rep(c('1999','2019','2040'),4)),
                      group = c(rep('EU',3),rep('US',3),rep('Asia',3),rep('Other',3)),
                      value=c(sum(t99new$Total[t99new$cat=='EU']),
                           sum(t40$Previous[t40$cat=='EU']),
                           sum(t40$Total[t40$cat=='EU']),
                           sum(t99new$Total[t99new$cat=='US']),
                           sum(t40$Previous[t40$cat=='US']),
                           sum(t40$Total[t40$cat=='US']),
                           sum(t99new$Total[t99new$cat=='Asia']),
                           sum(t40$Previous[t40$cat=='Asia']),
                           sum(t40$Total[t40$cat=='Asia']),
                           sum(t99new$Total[t99new$cat=='Other']),
                           sum(t40$Previous[t40$cat=='Other']),
                           sum(t40$Total[t40$cat=='Other'])))

bar <- ggplot(data=bardata, aes(fill=group, y=value, x=year)) + 
  geom_bar(position="fill", stat="identity")+
  theme_bw()+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),labels = scales::percent_format())+
  labs(title='Trade with Britain, % of total')+
  theme(legend.title = element_blank())+
  theme(axis.title = element_blank())

barborder <- plot_grid(bar) + theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
fig <- plot_grid(p1,p2,p3,barborder,ncol=2)

save_plot('plots/mapsandbar.png',fig,base_height = 8,base_width = 12,units = 'in') 

