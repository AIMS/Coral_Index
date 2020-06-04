
# discharge corrected for ungauged portions of rivers, estimated by multiplication of gauged flows with jcu correction factors

library(tidyverse)
library(mgcv)
library(MuMIn)
library(gridExtra)
library(ggplot2)
library(grid)

discharge.annual<-read.csv(file='dataRepository/discharge.annual.csv')
load(file='output/coral.RData')

#consitent plotting
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

######

dis<-discharge.annual %>% ungroup %>%
  group_by(subregion) %>%
  arrange(Year) %>%
  mutate(dis.lag=lag(discharge.c.annual),
         dis2=((dis.lag+discharge.c.annual)/2)/10000000) %>% # express in 10*cubic km to put on more appropriate scale
  ungroup %>%
  dplyr::select(subregion,Year,dis2) # dis2 is the mean discharge over the past two water years, includes end of water year post sampling..

regions<-unique(coral %>% dplyr::select(NRM_REGION, subregion))

dis.region<-dis %>% left_join(regions) %>% dplyr::select(-subregion) %>%
  group_by(NRM_REGION,Year) %>%
  summarise_all(sum)

coral.1 <-coral %>% 
  filter(yr>2006 & !is.na(MAprop)) %>% # removes carried forward scores, and pre 2007 as no change scores prior to this
  dplyr::select(NRM_REGION,subregion,REEF,DEPTH,yr,CoralCover.score, ma.score,gen.composition,composition.score,juv.score,Change.score.linear.mean,DISTURBANCE)
coral.1$index<-rowMeans(subset(coral.1, select=c(ma.score,juv.score,Change.score.linear.mean,composition.score,CoralCover.score)),na.rm=TRUE)
coral.1$NRM_REGION<-factor(coral.1$NRM_REGION)

coral.s<-coral.1 %>% 
  mutate(DISTURBANCE=as.factor(ifelse(DISTURBANCE=='f' & REEF %in% c('High West', 'Pine','Seaforth','Double Cone','Middle Rf'),'n',
                                      ifelse(DISTURBANCE=='f' & yr == '2008','n',as.character(DISTURBANCE))))) %>%
  rename(Year=yr)  %>%
  dplyr::select(NRM_REGION,subregion,REEF,DEPTH,Year,index, DISTURBANCE) %>%
  group_by(REEF,DEPTH) %>%
  arrange(REEF,DEPTH,Year) %>%
  mutate(index.lag=lag(index),
         index.lag2=lag(index,2),
         disturbance.lag=lag(DISTURBANCE),
         disturbance.lag2=lag(DISTURBANCE,2),
         year.lag=lag(Year),
         year.lag2=lag(Year,2),
         period1=Year-year.lag,
         period2=Year-year.lag2) %>%
  droplevels

coral.lag2a<-coral.s %>% 
  filter(period1=='2') %>%
  mutate(index.dif=ifelse(DISTURBANCE %in% c('s','m','c','b'), NA, (index-index.lag)/2)) %>%
  ungroup %>%
  dplyr::select(NRM_REGION,subregion,REEF,DEPTH,Year,index.dif) 

coral.lag2b<-coral.s %>% 
  filter(period2=='2') %>%
  mutate(index.dif=ifelse(DISTURBANCE %in% c('s','m','c','b') | disturbance.lag %in% c('s','m','c','b'), NA, (index-index.lag2)/2)) %>%
  dplyr::select(NRM_REGION,subregion,REEF,DEPTH,Year,index.dif) %>%
  data.frame()

coral.2yr<-rbind(coral.lag2a,coral.lag2b) 

coral.env <- coral.2yr %>% 
    left_join(dis.region)
  
xlabs = as.expression(Discharge~10~(km^{3}))
############## Wet TRopics

wet2<-coral.env %>% filter(NRM_REGION=="Wet Tropics")
  
gam.wet.dis<-gam(index.dif~s(dis2, k=4), data=wet2, family="gaussian") # edf=2.4, psuedo r-sq = 0.194
summary(gam.wet.dis)
plot(gam.wet.dis)


AICc(gam.wet.dis) #-274
#null.wet.dis<-gam(index.dif~1, data=wet2, family="gaussian") #-257
#lm.wet.dis<-gam(index.dif~dis2, data=wet2, family="gaussian") #-270
############ Burdekin

b2<-coral.env %>% filter(NRM_REGION=="Burdekin")
 
gam.b.dis<-gam(index.dif~s(dis2, k=4), data=b2, family="gaussian") # edf=1.6, psuedo r-sq = 0.166
summary(gam.b.dis)
plot(gam.b.dis)

#AICc(gam.b.dis) #-164
#null.b.dis<-gam(index.dif~1, data=b2, family="gaussian") 
lm.b.dis<-gam(index.dif~dis2, data=b2, family="gaussian") 
#AICc(lm.b.dis) #-163.6
#AICc(null.b.dis) #-156.3

############ Whitsundays
wh2<-coral.env %>% filter(NRM_REGION=="Mackay Whitsunday") 

 gam.wh.dis<-gam(index.dif~s(dis2, k=4), data=wh2, family="gaussian") # edf=1, psuedo r-sq = 0.051
 summary(gam.wh.dis)
 plot(gam.wh.dis)

 #AICc(gam.wh.dis) #-213.03
 #null.wh.dis<-gam(index.dif~1, data=wh2, family="gaussian") 
 #lm.wh.dis<-gam(index.dif~dis2, data=wh2, family="gaussian") 
 #AICc(lm.wh.dis) #-212.02
 #AICc(null.wh.dis) #-211.5
 
######Fitzroy

f2<-coral.env %>% filter(NRM_REGION=="Fitzroy") 

 gam.f.dis<-gam(index.dif~s(dis2, k=4), data=f2, family="gaussian") # edf=2.5, psuedo r-sq = 0.277
 summary(gam.f.dis)
 plot(gam.f.dis)

 AICc(gam.f.dis) #-171.2
 #null.f.dis<-gam(index.dif~1, data=f2, family="gaussian") 
 #lm.f.dis<-gam(index.dif~dis2, data=f2, family="gaussian") #-270
 #AICc(lm.f.dis) #-166.6
 #AICc(null.f.dis) #-156.7
 
############
#temporal plot index
##############

dis2.wet<-seq(min(wet2$dis2),max(wet2$dis2),l=1000)
pp2.wet<-predict(gam.wet.dis, newdata=data.frame(dis2=dis2.wet),type="response", se=T)
wt.lower <- as.vector(pp2.wet$fit) - as.vector(pp2.wet$se.fit*qt(0.975,df=df.residual(gam.wet.dis)))
wt.upper <- as.vector(pp2.wet$fit) + as.vector(pp2.wet$se.fit*qt(0.975,df=df.residual(gam.wet.dis)))
wt.fit <- as.vector(pp2.wet$fit)

newdata.wt <- data.frame(dis=dis2.wet,
                           fit=wt.fit, 
                           lower=wt.lower, 
                           upper=wt.upper)

dis2.b<-seq(min(b2$dis2),max(b2$dis2),l=1000)
pp2.b<-predict(gam.b.dis, newdata=data.frame(dis2=dis2.b),type="response", se=T)
b.lower <- as.vector(pp2.b$fit) - as.vector(pp2.b$se.fit*qt(0.975,df=df.residual(gam.b.dis)))
b.upper <- as.vector(pp2.b$fit) + as.vector(pp2.b$se.fit*qt(0.975,df=df.residual(gam.b.dis)))
b.fit <- as.vector(pp2.b$fit)

newdata.b <- data.frame(dis=dis2.b,
                         fit=b.fit, 
                         lower=b.lower, 
                         upper=b.upper)

dis2.wh<-seq(min(wh2$dis2),max(wh2$dis2),l=1000)
pp2.wh<-predict(gam.wh.dis, newdata=data.frame(dis2=dis2.wh),type="response", se=T)
wh.lower <- as.vector(pp2.wh$fit) - as.vector(pp2.wh$se.fit*qt(0.975,df=df.residual(gam.wh.dis)))
wh.upper <- as.vector(pp2.wh$fit) + as.vector(pp2.wh$se.fit*qt(0.975,df=df.residual(gam.wh.dis)))
wh.fit <- as.vector(pp2.wh$fit)

newdata.wh <- data.frame(dis=dis2.wh,
                         fit=wh.fit, 
                         lower=wh.lower, 
                         upper=wh.upper)

dis2.f<-seq(min(f2$dis2),max(f2$dis2),l=1000)
pp2.f<-predict(gam.f.dis, newdata=data.frame(dis2=dis2.f),type="response", se=T)
f.lower <- as.vector(pp2.f$fit) - as.vector(pp2.f$se.fit*qt(0.975,df=df.residual(gam.f.dis)))
f.upper <- as.vector(pp2.f$fit) + as.vector(pp2.f$se.fit*qt(0.975,df=df.residual(gam.f.dis)))
f.fit <- as.vector(pp2.f$fit)

newdata.f <- data.frame(dis=dis2.f,
                        fit=f.fit, 
                        lower=f.lower, 
                        upper=f.upper)

#######
# Plots
#######

wt <- ggplot(newdata.wt, aes(y=fit, x=dis)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=dis), fill='grey',alpha=0.5)+
  geom_line(aes(y=fit, x=dis), color='black')+
  geom_point(aes(y=index.dif, x=dis2), data=wet2, size=1, colour='darkgrey')+
  scale_x_continuous(xlabs[1],, breaks=c(2,3,4))+
  scale_y_continuous('Change in index score',limits=c(-0.2,0.2))+
  ggtitle("Wet Tropics")+
  theme_a()+
  annotate(geom='text', x=-Inf,y= 0.19, label='a)', size=Size, hjust=-1, vjust=0)+
  annotate(geom='text', x=2.5,y=-0.18, label='paste(italic(R)^2,\" = 0.194")',parse=TRUE, size=Size)+
  geom_hline(yintercept=0, color='black', linetype='dashed')

bd <- ggplot(newdata.b, aes(y=fit, x=dis)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=dis), fill='grey',alpha=0.5)+
  geom_line(aes(y=fit, x=dis), color='black')+
  geom_point(aes(y=index.dif, x=dis2), data=b2, size=1, colour='darkgrey')+
  scale_x_continuous(xlabs[1])+
  scale_y_continuous('Change in index score',limits=c(-0.2,0.2))+
  ggtitle("Burdekin")+
  theme_a()+
  annotate(geom='text', x=-Inf,y= 0.19, label='b)', size=Size, hjust=-1, vjust=0)+
  annotate(geom='text', x=1.2,y=-0.18, label='paste(italic(R)^2,\" = 0.166")',parse=TRUE, size=Size)+
  geom_hline(yintercept=0, color='black', linetype='dashed')

wh <- ggplot(newdata.wh, aes(y=fit, x=dis)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=dis), fill='grey',alpha=0.5)+
  geom_line(aes(y=fit, x=dis), color='black')+
  geom_point(aes(y=index.dif, x=dis2), data=wh2, size=1, colour='darkgrey')+
  scale_x_continuous(xlabs[1])+
  scale_y_continuous('Change in index score',limits=c(-0.2,0.2))+
  ggtitle("Mackay Whitsunday")+
  theme_a()+
    annotate(geom='text', x=-Inf,y= 0.19, label='c)', size=Size, hjust=-1, vjust=0)+
  annotate(geom='text', x=0.4,y=-0.18, label='paste(italic(R)^2,\" = 0.051")',parse=TRUE, size=Size)+
  geom_hline(yintercept=0, color='black', linetype='dashed')

f <- ggplot(newdata.f, aes(y=fit, x=dis)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=dis), fill='grey',alpha=0.5)+
  geom_line(aes(y=fit, x=dis), color='black')+
  geom_point(aes(y=index.dif, x=dis2), data=f2, size=1, colour='darkgrey')+
  scale_y_continuous('Change in index score',limits=c(-0.25,0.15))+
  ggtitle("Fitzroy")+
  theme_a()+
  annotate(geom='text', x=-Inf,y= 0.14, label='d)', size=Size, hjust=-1, vjust=0)+
  annotate(geom='text', x=1,y=-0.23, label='paste(italic(R)^2,\" = 0.277")',parse=TRUE, size=Size)+
  geom_hline(yintercept=0, color='black', linetype='dashed')


b = textGrob(label=expression(Discharge~("10"~km^3)), gp=gpar(fontsize=7))
############# FIGURE 4****************************************************************
ggsave('output/Figure4 1column.png', width=90, height=100, units="mm", dpi=600,
       plot=grid.arrange(wt, bd, wh, f, nrow=2, bottom=b, 
                         left=textGrob(label='Change in index score',  gp=gpar(fontsize=7), rot=90)))

ggsave('output/Figure4 1column.jpg', width=90, height=100, units="mm", dpi=600,
       plot=grid.arrange(wt, bd, wh, f, nrow=2, bottom=b, 
                         left=textGrob(label='Change in index score',  gp=gpar(fontsize=7), rot=90)))

ggsave('output/Figure4 1column.pdf', width=90, height=100, units="mm", dpi=600,
       plot=grid.arrange(wt, bd, wh, f, nrow=2, bottom=b, 
                         left=textGrob(label='Change in index score',  gp=gpar(fontsize=7), rot=90)))
####***********************************************************************************

