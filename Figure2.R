
#Coral Worm plot with disturubances for paper

library(gtable)
library(gridExtra)
library(grid)
library(lubridate)
library(tidyverse)

load(file='output/coral.RData')


#get the major disturbances for inclusion on index plot. Where disturbances are recorded as mixed == 'm' recode to identifiy
# the disturbance of primary importance. Note that early classifications as flood =='f' were due to high levels sediment and or disease rather than freshwater kill as applied later in the time-series
dist<-coral %>% 
  mutate(DISTURBANCE=ifelse(DISTURBANCE=='m' & NRM_REGION=="Fitzroy", 's',
                            ifelse(DISTURBANCE=='m' & NRM_REGION=="Wet Tropics" & yr=='2017','b',
                                   ifelse(DISTURBANCE=='m' & NRM_REGION=="Wet Tropics" & yr=='2011','s',
                                          ifelse(DISTURBANCE=='f' & REEF %in% c('Pine','Seaforth','Double Cone','Middle Rf'),'n',as.character(DISTURBANCE)))))) %>%
  filter(yr >2005 & !DISTURBANCE %in% c('n','d','unk') & !is.na(DISTURBANCE)) %>%
  dplyr::select(NRM_REGION,DISTURBANCE,yr) %>% 
  mutate(obs=1) %>%
  group_by(NRM_REGION,DISTURBANCE,yr)%>%
  summarise(obs=sum(obs)) %>%
  filter(obs>2) %>% 
  rename(Region=NRM_REGION)

print(dist)

load(file='output/boot.region.metric.sum.RData')
load(file='output/boot.region.score.sum.RData')
# min=as.Date("2005-01-01")
# max=as.Date("2018-01-08")
# CoralDateRanges<-data.frame(min,max)
trafficLightPalette <- (c('#FF0000','#FFC000','#FFFF00','#92D050','#00B050'))
lims <- rev(LETTERS[1:5]) 

index.scores=boot.region.metric.sum
indicators=boot.region.score.sum

theme_a<-function(base_size=7){
  theme_classic(base_size=base_size) +
    theme(strip.background=element_blank(),
          strip.text=element_blank(),
          axis.title.x=element_blank(),
          plot.margin=unit(c(0.5,0.5,0.5,0.5),"lines"),
          legend.margin=unit(c(0.1,0.1,0.1,0.1),"lines"),
          legend.spacing.y = unit(0.5,"lines"),
          panel.spacing=unit(c(0),"lines"),
          axis.title.y=element_blank(),
          axis.text.x=element_text(size=7),
          axis.text.y=element_text(size=7),
          plot.title = element_text(hjust=0.5, size=rel(1)),
          axis.line.x=element_line(),axis.line.y=element_line(),
          legend.position="bottom",
          legend.title = element_blank()
           )
}

#conversion for size back to points
Size=0.3528*7


################
region="Wet Tropics"

dat <- index.scores %>% filter(Region==region)
ind.dat <- indicators %>% filter(Region==region)
ti=region

dtt <- filter(dat, Year>2004) %>% droplevels
dt <- filter(ind.dat, Year>2004)

p1 <- ggplot(dtt, aes(y=Mean, x=Year)) +
  geom_hline(yintercept=seq(0,1,by=0.2), color='grey80', linetype='dashed')+
  scale_x_continuous('',breaks=c(2005,2010,2015),limits=c(2005,2017.5))+
  scale_y_continuous('', breaks=seq(0,1,by=0.2), limits=c(0,1),expand=c(0,0))+
  scale_fill_manual('', values=trafficLightPalette, limits=lims, guide=FALSE)+
  ggtitle(region)+
  theme_a()+
  theme(legend.key.height=unit(0.8,"line"))
  
p1 <- p1+
  annotate('rect', xmin=2012, xmax=2015, ymin=0, ymax=0.9, alpha=0.2)+
  annotate('rect', xmin=2005.8, xmax=2006.2, ymin=0, ymax=0.90, alpha=0.15, fill='black')+
  annotate('rect', xmin=2016.8, xmax=2017.2, ymin=0, ymax=0.90, alpha=0.15, fill='black')+
  annotate('rect', xmin=2010.8, xmax=2011.2, ymin=0, ymax=0.90, alpha=0.15, fill='black')+
  annotate('rect', xmin=2013.8, xmax=2014.2, ymin=0, ymax=0.9, alpha=0.15, fill='black')+
  annotate('text',x=2014, y=0.95, label='s', size=Size)+
  annotate('text',x=2013.5, y=0.95, label='c', size=Size)+
  annotate('text',x=2017, y=0.95, label='b', size=Size)+
  annotate('text',x=2006, y=0.95, label='s', size=Size)+
  annotate('text',x=2011, y=0.95, label='s', size=Size)+
  geom_line(data=dt, aes(y=Mean, x=Year,color=Score), size=0.5)+
  scale_color_manual('Indicator',breaks=c('Coral change', 'Coral composition', 'Coral cover', 'Juvenile density', 'Macroalgal cover','Overall'),
                     #values=c('red','blue','green','magenta','tan', 'black')) 
                     values=c('#E69F00','#009E73','#007282','#F0E442','#CC79A7','black')) 

p1<-p1+
  guides(color=guide_legend(nrow=2 ,byrow=F))

#add the index scores
p1=p1+geom_blank(data=dt, aes(y=Mean, x=Year))+
  theme(legend.key.width=unit(1,'cm'),
        legend.text=element_text(margin=margin(r=15, unit='pt')))+
  geom_linerange(aes(ymin=lower, ymax=upper))+
  geom_line(size=0.5) + 
  geom_point(data=dtt,aes(fill=Grade), shape=21, size=3.5)

#############
region="Burdekin"

dtt <- index.scores %>% filter(Region==region & Year>2004)
dt <- indicators %>% filter(Region==region & Year>2004)
ti=region

p2 <- ggplot(dtt, aes(y=Mean, x=Year)) +
  geom_hline(yintercept=seq(0,1,by=0.2), color='grey80', linetype='dashed')+
  scale_x_continuous('',breaks=c(2005,2010,2015),limits=c(2005,2017.5))+
  scale_y_continuous('', breaks=seq(0,1,by=0.2), limits=c(0,1),expand=c(0,0))+
  scale_fill_manual('', values=trafficLightPalette, limits=lims, guide=FALSE)+
  ggtitle(region)+
  theme_a()

p2 <- p2+
  annotate('rect', xmin=2005.8, xmax=2006.2, ymin=0, ymax=0.9, alpha=0.15, fill='black')+
  annotate('rect', xmin=2016.8, xmax=2017.2, ymin=0, ymax=0.9, alpha=0.15, fill='black')+
  annotate('rect', xmin=2010.8, xmax=2011.2, ymin=0, ymax=0.9, alpha=0.15, fill='black')+
  annotate('text',x=2017, y=0.95, label='b', size=Size)+
  annotate('text',x=2006, y=0.95, label='s', size=Size)+
  annotate('text',x=2011, y=0.95, label='s', size=Size)+
  geom_line(data=dt, aes(y=Mean, x=Year,color=Score), size=0.5)+
  scale_color_manual('Indicator',breaks=c('Coral change', 'Coral composition', 'Coral cover', 'Juvenile density', 'Macroalgal cover','Overall'),
                     values=c('#E69F00','#009E73','#007282','#F0E442','#CC79A7','black')) +
                     #values=c('red','blue','green','magenta','tan', 'black')) +
  guides(color=guide_legend(nrow=2 ,byrow=F))

#add the index scores
p2=p2+geom_blank(data=dt, aes(y=Mean, x=Year, shape='Overall', linetype='Overall', color='Overall', size='Overall'))+
  theme(legend.key.width=unit(1,'cm'),
        legend.text=element_text(margin=margin(r=15, unit='pt')))+
  geom_linerange(aes(ymin=lower, ymax=upper))+
  geom_line(size=0.5) + 
  geom_point(data=dtt,aes(fill=Grade), shape=21, size=3.5)
  

#############
region="Mackay Whitsunday"

dtt <- index.scores %>% filter(Region==region & Year>2004)
dt <- indicators %>% filter(Region==region & Year>2004)
ti=region

p3 <- ggplot(dtt, aes(y=Mean, x=Year)) +
  geom_hline(yintercept=seq(0,1,by=0.2), color='grey80', linetype='dashed')+
  scale_x_continuous('',breaks=c(2005,2010,2015),limits=c(2005,2017.5))+
  scale_y_continuous('', breaks=seq(0,1,by=0.2), limits=c(0,1),expand=c(0,0))+
  scale_fill_manual('', values=trafficLightPalette, limits=lims, guide=FALSE)+
  ggtitle(region)+
  theme_a()

p3 <- p3+
  annotate('rect', xmin=2016.8, xmax=2017.2, ymin=0, ymax=0.9, alpha=0.15, fill='black')+
  annotate('rect', xmin=2009.8, xmax=2010.2, ymin=0, ymax=0.9, alpha=0.15, fill='black')+
  annotate('text',x=2010, y=0.95, label='s', size=Size)+
  annotate('text',x=2017, y=0.95, label='s', size=Size)+
  geom_line(data=dt, aes(y=Mean, x=Year,color=Score), size=0.5)+
  scale_color_manual('Indicator',breaks=c('Coral change', 'Coral composition', 'Coral cover', 'Juvenile density', 'Macroalgal cover','Overall'),
                     values=c('#E69F00','#009E73','#007282','#F0E442','#CC79A7','black')) +
                     #values=c('red','blue','green','magenta','tan', 'black'))+
  guides(color=guide_legend(nrow=2 ,byrow=F))

#add the index scores
p3=p3+geom_blank(data=dt, aes(y=Mean, x=Year, shape='Overall', linetype='Overall', color='Overall', size='Overall'))+
  theme(legend.key.width=unit(1,'cm'),
        legend.text=element_text(margin=margin(r=15, unit='pt')))+
  geom_linerange(aes(ymin=lower, ymax=upper))+
  geom_line(size=0.5) + 
  geom_point(data=dtt,aes(fill=Grade), shape=21, size=3.5)
 

#############
region="Fitzroy"
#Fitzroy  2006(b), 2011(f), 2010&2015(s)

dtt <- index.scores %>% filter(Region==region & Year>2004)
dt <- indicators %>% filter(Region==region & Year>2004)
ti=region

p4 <- ggplot(dtt, aes(y=Mean, x=Year)) +
  geom_hline(yintercept=seq(0,1,by=0.2), color='grey80', linetype='dashed')+
  scale_x_continuous('',breaks=c(2005,2010,2015),limits=c(2005,2017.5))+
  scale_y_continuous('', breaks=seq(0,1,by=0.2), limits=c(0,1),expand=c(0,0))+
  scale_fill_manual('', values=trafficLightPalette, limits=lims, guide=FALSE)+
  ggtitle(region)+
  theme_a()

p4 <- p4+
  annotate('rect', xmin=2005.8, xmax=2006.2, ymin=0, ymax=0.9, alpha=0.15, fill='black')+
  annotate('rect', xmin=2009.8, xmax=2010.2, ymin=0, ymax=0.9, alpha=0.15, fill='black')+
  annotate('rect', xmin=2014.8, xmax=2015.2, ymin=0, ymax=0.9, alpha=0.15, fill='black')+
  annotate('rect', xmin=2010.8, xmax=2011.2, ymin=0, ymax=0.9, alpha=0.15, fill='black')+
  annotate('text',x=2006, y=0.95, label='b', size=Size)+
  annotate('text',x=2010, y=0.95, label='s', size=Size)+
  annotate('text',x=2011, y=0.95, label='f', size=Size)+
  annotate('text',x=2015, y=0.95, label='s', size=Size)+
  geom_line(data=dt, aes(y=Mean, x=Year,color=Score),size=0.5)+
  scale_color_manual('Indicator',breaks=c('Coral change', 'Coral composition', 'Coral cover', 'Juvenile density', 'Macroalgal cover','Overall'),
                     values=c('#E69F00','#009E73','#007282','#F0E442','#CC79A7','black')) +
                    #values=c('red','blue','green','magenta','tan', 'black'))+
  guides(color=guide_legend(nrow=2 ,byrow=F))
  

#add the index scores
p4=p4+geom_blank(data=dt, aes(y=Mean, x=Year, shape='Overall', linetype='Overall', color='Overall', size='Overall'))+
  theme(legend.key.width=unit(1,'cm'),
        legend.text=element_text(margin=margin(r=15, unit='pt')))+
  geom_linerange(aes(ymin=lower, ymax=upper))+
  geom_line(size=0.5) + 
  geom_point(data=dtt,aes(fill=Grade), shape=21, size=3.5)




g_legend<-function(p1){
  tmp <- ggplot_gtable(ggplot_build(p1))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

yLab=textGrob(label='Index or indicator score', gp=gpar(fontsize=8), rot=90)



ggsave('output/Figure2 1.5column.png', width=140, height=110, units="mm", dpi=600,
plot=grid.arrange(arrangeGrob(p1+ theme(legend.position='none'), 
                         p2+ theme(legend.position='none'), 
                         p3+ theme(legend.position='none'), 
                         p4+ theme(legend.position='none'), 
                         left=yLab),
                  g_legend(p1 + theme(legend.position='bottom')),
                  ncol=1, nrow=2,heights=c(1.8,0.2)))

ggsave('output/Figure2 1.5column.pdf', width=140, height=110, units="mm", dpi=600,
       plot=grid.arrange(arrangeGrob(p1+ theme(legend.position='none'), 
                                     p2+ theme(legend.position='none'), 
                                     p3+ theme(legend.position='none'), 
                                     p4+ theme(legend.position='none'), 
                                     left=yLab),
                         g_legend(p1 + theme(legend.position='bottom')),
                         ncol=1, nrow=2,heights=c(1.8,0.2)))

ggsave('output/Figure2 1.5column.jpg', width=140, height=110, units="mm", dpi=600,
       plot=grid.arrange(arrangeGrob(p1+ theme(legend.position='none'), 
                                     p2+ theme(legend.position='none'), 
                                     p3+ theme(legend.position='none'), 
                                     p4+ theme(legend.position='none'), 
                                     left=yLab),
                         g_legend(p1 + theme(legend.position='bottom')),
                         ncol=1, nrow=2,heights=c(1.8,0.2)))
