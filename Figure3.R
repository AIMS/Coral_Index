
library(tidyverse)
library(cowplot) #plot_grid
library(gridExtra)
library(grid)
load(file='coral.RData') 

#consistent plotting parameters
theme_a<-function(base_size=7){
  theme_classic(base_size=base_size) +
    theme(strip.background=element_blank(),
          strip.text=element_blank(),
          axis.title.x=element_blank(),
          plot.margin=unit(c(0.5,0.5,0,0.5),"lines"),
         # legend.margin=unit(c(0.1,0.1,0.1,0.1),"lines"),
         # legend.spacing.y = unit(0.5,"lines"),
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
# summary table by disturbance
######
# processing of disturbance categorisations to represent: 1. primary disturbance type when mixed pressures were recorded
# and 2 recode flood pressures recorded in early records based on observed high levels of sedimentation rather than 
# freshwater kill as used for this category in later years.
hc.dist<-coral %>% filter(yr>2004 & !is.na(CoralCover)) %>% dplyr::select(NRM_REGION,REEF,DEPTH,yr,HC,CoralCover,DISTURBANCE) %>%
  droplevels() %>%
  group_by(NRM_REGION,REEF,DEPTH) %>%
  arrange(NRM_REGION,REEF,DEPTH,yr)%>%
  mutate(HClost=lag(HC)-HC,
         DISTURBANCE=as.character(DISTURBANCE),
         HC.p.lost=((lag(HC)-HC)/lag(HC))*100,
         Cover.lost=lag(CoralCover)-CoralCover,
         Cover.lost.p=((lag(CoralCover)-CoralCover)/lag(CoralCover))*100,
         DISTURBANCE=ifelse(DISTURBANCE=='m' & NRM_REGION=="Fitzroy", 's',
                            ifelse(DISTURBANCE=='m' & NRM_REGION=="Wet Tropics" & yr=='2017','b',
                                   ifelse(DISTURBANCE=='m' & NRM_REGION=="Wet Tropics" & yr=='2011','s',
                                          ifelse(DISTURBANCE=='f' & REEF %in% c('Pine','Seaforth','Double Cone','Middle Rf'),'n',DISTURBANCE))))) %>%
  ungroup

hc.dist.summary<-hc.dist %>% 
  group_by(NRM_REGION, DISTURBANCE) %>%
  summarise(HClost=mean(HClost),
            HC.p.lost=mean(HC.p.lost),
            Cover.lost=mean(Cover.lost),
            Cover.lost.p=mean(Cover.lost.p),
            obs=n()) %>%
  ungroup
            

coral.a<-coral %>% filter(yr>2004)
coral.s<-coral.a %>% dplyr::select(REEF,DEPTH,yr,CoralCover.score, MAprop,ma.score,gen.composition,composition.score,juv.score,Change.score.linear.mean,DISTURBANCE,CoralCover)
coral.s$index<-rowMeans(subset(coral.s, select=c(ma.score,juv.score,Change.score.linear.mean,composition.score,CoralCover.score)),na.rm=TRUE)


coral.b<-coral.s %>% 
  mutate(DISTURBANCE=as.factor(ifelse(DISTURBANCE=='f' & REEF %in% c('Pine','Seaforth','Double Cone','Middle Rf'),'n',
                                      ifelse(DISTURBANCE=='f' & yr == '2008','n',
                                      ifelse(DISTURBANCE=='d','n',as.character(DISTURBANCE)))))) # removes classifications of'f' where clearly no freshwater kill and 
                                                                                                # considers disease in the chonic influence


index.lag<- coral.b %>% dplyr::select(REEF,DEPTH,yr,index,Change.score.linear.mean,MAprop,DISTURBANCE,
                                     composition.score, juv.score, CoralCover.score,ma.score) %>%
  droplevels() %>%
  group_by(REEF, DEPTH) %>% 
  arrange(REEF,DEPTH,yr)%>%
  mutate(index.lag=lag(index),
         change.lag=lag(Change.score.linear.mean),
         comp.lag=lag(composition.score),
         cover.lag=lag(CoralCover.score),
         juv.lag=lag(juv.score),
         ma.lag=lag(ma.score)) %>%
  filter(!is.na(MAprop)) %>%
  filter(DISTURBANCE!="") %>%
  ungroup
  
index.change<-index.lag %>%  
   mutate(index.dif=index-index.lag,
         change.dif=Change.score.linear.mean-change.lag,
         juv.dif=juv.score-juv.lag,
         ma.dif=ma.score-ma.lag,
         cover.dif=CoralCover.score-cover.lag,
         comp.dif=composition.score-comp.lag)

change.dist<-index.change %>% dplyr::select(REEF,DEPTH,yr,DISTURBANCE,index.dif,change.dif,juv.dif,ma.dif,cover.dif,comp.dif) %>%
  ungroup %>%
  group_by(REEF,DEPTH,yr,DISTURBANCE) %>%
  gather(index.dif:comp.dif,key=metric, value=difference) %>%
  ungroup %>%
  group_by(REEF,DEPTH,yr,metric) %>%
  spread(DISTURBANCE, value=difference) %>%
  ungroup %>%
  mutate(metric=factor(metric, levels=c('index.dif', 'change.dif','comp.dif','cover.dif','juv.dif','ma.dif')))

#######
# linear models for intercept of score change distributions
######
summary(lm(b~1, change.dist %>% filter(!is.na(b) & metric=='index.dif'))) #*
summary(lm(b~1, change.dist %>% filter(!is.na(b) & metric=='comp.dif'))) #ns
summary(lm(b~1, change.dist %>% filter(!is.na(b) & metric=='cover.dif'))) #***
summary(lm(b~1, change.dist %>% filter(!is.na(b) & metric=='juv.dif'))) #***
summary(lm(b~1, change.dist %>% filter(!is.na(b) & metric=='ma.dif'))) #ns

labs.b=change.dist %>% filter(!is.na(b) & metric!='change.dif') %>% group_by(metric) %>%
  summarize(M=max(b)+0.05) %>%
  ungroup %>%
  mutate(Lab=c('*','','*','*',''))

summary(lm(c~1, change.dist %>% filter(!is.na(c) & metric=='index.dif'))) #*
summary(lm(c~1, change.dist %>% filter(!is.na(c) & metric=='comp.dif'))) #ns
summary(lm(c~1, change.dist %>% filter(!is.na(c) & metric=='cover.dif'))) #**
summary(lm(c~1, change.dist %>% filter(!is.na(c) & metric=='juv.dif'))) #ns
summary(lm(c~1, change.dist %>% filter(!is.na(c) & metric=='ma.dif'))) #ns

labs.c=change.dist %>% filter(!is.na(c) & metric!='change.dif') %>% group_by(metric) %>%
  summarize(M=max(c)+0.05) %>%
  ungroup %>%
  mutate(Lab=c('*','','*','',''))

summary(lm(f~1, change.dist %>% filter(!is.na(f) & metric=='index.dif'))) #ns
summary(lm(f~1, change.dist %>% filter(!is.na(f) & metric=='comp.dif'))) #ns
summary(lm(f~1, change.dist %>% filter(!is.na(f) & metric=='cover.dif'))) #*
summary(lm(f~1, change.dist %>% filter(!is.na(f) & metric=='juv.dif'))) #ns
summary(lm(f~1, change.dist %>% filter(!is.na(f) & metric=='ma.dif'))) #ns

labs.f=change.dist %>% filter(!is.na(f) & metric!='change.dif') %>% group_by(metric) %>%
  summarize(M=max(f)+0.05) %>%
  ungroup %>%
  mutate(Lab=c('','','*','',''))

summary(lm(s~1, change.dist %>% filter(!is.na(s) & metric=='index.dif'))) #***
summary(lm(s~1, change.dist %>% filter(!is.na(s) & metric=='comp.dif'))) #***
summary(lm(s~1, change.dist %>% filter(!is.na(s) & metric=='cover.dif'))) #***
summary(lm(s~1, change.dist %>% filter(!is.na(s) & metric=='juv.dif'))) #***
summary(lm(s~1, change.dist %>% filter(!is.na(s) & metric=='ma.dif'))) #***

labs.s=change.dist %>% filter(!is.na(s) & metric!='change.dif') %>% group_by(metric) %>%
  summarize(M=max(s)+0.05) %>%
  ungroup %>%
  mutate(Lab=c('*','*','*','*','*'))

summary(lm(n~1, change.dist %>% filter(!is.na(n) & metric=='index.dif'))) #*
summary(lm(n~1, change.dist %>% filter(!is.na(n) & metric=='change.dif'))) #ns
summary(lm(n~1, change.dist %>% filter(!is.na(n) & metric=='comp.dif'))) #.
summary(lm(n~1, change.dist %>% filter(!is.na(n) & metric=='cover.dif'))) #***
summary(lm(n~1, change.dist %>% filter(!is.na(n) & metric=='juv.dif'))) #***
summary(lm(n~1, change.dist %>% filter(!is.na(n) & metric=='ma.dif'))) #***

labs.n=change.dist %>% filter(!is.na(n)) %>% group_by(metric) %>%
  summarize(M=max(n)+0.05) %>%
  ungroup %>%
  mutate(Lab=c('*','','','*','*','*'))
  
#boxplots
bp1<- change.dist %>% filter(!is.na(b) & metric!='change.dif') %>%
      ggplot(aes(y=b, x=metric)) + geom_hline(yintercept=0, linetype="dashed") +
  geom_violin() +
  geom_point(alpha=0.1, colour='black', size=1.3)+
  scale_y_continuous('Score change', lim=c(-1,1.15)) +
  scale_x_discrete('', breaks=c('index.dif','cover.dif','juv.dif','ma.dif','comp.dif'), 
                   labels=c('I','Cv','J','A','Cm'))+
  theme_a()+
  annotate(geom='text', x=-Inf,y= 0.95, label='a)', size=Size, hjust=-1, vjust=0)+
  geom_text(data=labs.b, aes(y=M, x=metric, label=Lab), size=0.3528*16) +
  ggtitle("Bleaching")

 

bp2<- change.dist %>% filter(!is.na(c) & metric!='change.dif') %>%
  ggplot(aes(y=c, x=metric)) + geom_hline(yintercept=0, linetype="dashed") +
  geom_violin() +
  geom_point(alpha=0.1, colour='black', size=1.3)+
  geom_text(data=labs.c, aes(y=M, x=metric, label=Lab), size=0.3528*16) +
  scale_y_continuous('', lim=c(-1,1.15)) +
  scale_x_discrete('', breaks=c('index.dif','cover.dif','juv.dif','ma.dif','comp.dif'), 
                   labels=c('I','Cv','J','A','Cm'))+
  theme_a()+
  annotate(geom='text', x=-Inf,y= 0.95, label='b)', size=Size, hjust=-1, vjust=0)+
  ggtitle("Crown-of-thorns")

bp3<- change.dist %>% filter(!is.na(f) & metric!='change.dif') %>%
  ggplot(aes(y=f, x=metric)) + geom_hline(yintercept=0, linetype="dashed") +
  geom_violin() +
  geom_point(alpha=0.1, colour='black', size=1.3)+
  geom_text(data=labs.f, aes(y=M, x=metric, label=Lab), size=0.3528*16) +
  scale_y_continuous('', lim=c(-1,1.15)) +
  scale_x_discrete('', breaks=c('index.dif','cover.dif','juv.dif','ma.dif','comp.dif'), 
                   labels=c('I','Cv','J','A','Cm'))+
  theme_a()+
  annotate(geom='text', x=-Inf,y= 0.95, label='c)', size=Size, hjust=-1, vjust=0)+
  ggtitle("Floods")

bp4<- change.dist %>% filter(!is.na(s)& metric!='change.dif') %>%
  ggplot(aes(y=s, x=metric)) + geom_hline(yintercept=0, linetype="dashed") +
  geom_violin() +
  geom_point(alpha=0.1, colour='black', size=1.3)+
  geom_text(data=labs.s, aes(y=M, x=metric, label=Lab), size=0.3528*16) +
  scale_y_continuous('',lim=c(-1,1.15)) +
  scale_x_discrete('', breaks=c('index.dif','cover.dif','juv.dif','ma.dif','comp.dif'), 
                   labels=c('I','Cv','J','A','Cm'))+
  theme_a()+
  annotate(geom='text', x=-Inf,y= 0.95, label='d)', size=Size, hjust=-1, vjust=0)+
  ggtitle("Cyclones")

bp5<- change.dist %>% filter(!is.na(n)) %>%
  ggplot(aes(y=n, x=metric)) + geom_hline(yintercept=0, linetype="dashed") +
  geom_violin() +
  geom_point(alpha=0.1, colour='black', size=1.3)+
  geom_text(data=labs.n, aes(y=M, x=metric, label=Lab), size=0.3528*16) +
  scale_y_continuous('',lim=c(-1,1.15)) +
  scale_x_discrete('', breaks=c('index.dif','change.dif','cover.dif','juv.dif','ma.dif','comp.dif'), 
                   labels=c('I','Ch','Cv','J','A','Cm'))+
  ggtitle("No acute disturbance")+
  theme_a()+
  annotate(geom='text', x=-Inf,y= 0.95, label='e)', size=Size, hjust=-1, vjust=0)

#####
# output the plot
#####

lab.dat<-data.frame(x=seq(0,10, by=0.2), y=seq(0,10, by=0.2))
lab.p<-ggplot(lab.dat,aes(y=y, x=x)) +
  theme_nothing()+
  scale_y_continuous('',lim=c(0,10))+
  scale_x_continuous('',lim=c(0,10))+
  annotate(geom='text', x=1,y= 8, label='Score Labels', size=Size, hjust="left")+
  annotate(geom='text', x=1,y= 7.2, label='I - Index', size=Size, hjust="left")+
  annotate(geom='text', x=1,y= 6.4, label='Cv - Coral Cover', size=Size, hjust="left")+
  annotate(geom='text', x=1,y= 5.6, label='J - Juveniles', size=Size, hjust="left")+
  annotate(geom='text', x=1,y= 4.8, label='Cm - Composition', size=Size, hjust="left")+
  annotate(geom='text', x=1,y= 4, label='A - Macroalgae', size=Size, hjust="left")+
  annotate(geom='text', x=1,y= 3.2, label='Ch - Cover Change', size=Size, hjust="left")

top_row<-plot_grid(bp1, bp2, rel_widths=c(1,1), nrow=1)
mid_row<-plot_grid(bp3, bp4, rel_widths=c(1,1), nrow=1)
bot_row<-plot_grid(bp5,lab.p, rel_widths=c(1.162,0.838), nrow=1)

############# FIGURE 3****************************************************************
ggsave('output/Figure3 1column.png', width=90, height=150, units="mm", dpi=600,
       plot=grid.arrange(top_row, mid_row, bot_row, nrow=3, 
                         left=grid.text(label = 'Change in score', rot=90, gp=gpar(fontsize=7))))

ggsave('output/Figure3 1column.pdf', width=90, height=150, units="mm", dpi=600,
       plot=grid.arrange(chl.2.pAC,tss.2.pAC,
                         chl.5.pAC,tss.5.pAC,
                         nrow=2))

ggsave('output/Figure3 1column.jpg', width=90, height=150, units="mm", dpi=600,
       plot=grid.arrange(chl.2.pAC,tss.2.pAC,
                         chl.5.pAC,tss.5.pAC,
                         nrow=2))
####***********************************************************************************


#### Table 4 loss by disturbance
mag.change<-index.change %>% ungroup %>%
  dplyr::select(DISTURBANCE,index.dif,juv.dif,ma.dif,cover.dif,comp.dif) %>%
  gather(indicator, change, -DISTURBANCE) %>%
  filter(change<0) %>% 
  group_by(DISTURBANCE,indicator) %>%
  summarise(change.t=sum(change),
            number=n()) %>% ungroup

total_loss<-mag.change %>% 
  dplyr::select(-number) %>%
  group_by(indicator) %>%
  summarise(total.loss=sum(change.t))


prop.change<-mag.change %>% left_join(total_loss) %>%
  mutate(prop=round(change.t/total.loss,3)*100,
         tab.v=paste0(prop, "%"," ", "(",number,")")) %>%
  dplyr::select(DISTURBANCE,indicator,tab.v) %>%
  group_by(indicator) %>% 
  spread(DISTURBANCE,tab.v) %>%
  ungroup %>%
  dplyr::select(-unk)%>%
  mutate(indicator=factor(indicator, 
                          levels=c('index.dif','cover.dif','juv.dif','comp.dif','ma.dif'),
                          labels=c('Index', 'Coral Cover','Juveniles','Composition','MAcroalgae'))) %>%
  arrange(indicator) %>%
  dplyr::select('indicator','s','b','c','f','m','n')
write.csv(prop.change, file="output/Table4.csv")
