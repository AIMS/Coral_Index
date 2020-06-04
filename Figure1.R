
library(tidyverse)
library(gamlss)
library(MuMIn)
library(gridExtra)

load(file='output/coral.RData')
bom.month<-read.csv(file='dataRepository/bom.month.csv')
#### consistent theme for plots
theme_a<-function(base_size=7){
  theme_classic(base_size=base_size) +
    theme(strip.background=element_blank(),
          strip.text=element_blank(),
          #axis.title.x=element_blank(),
          plot.margin=unit(c(0.5,0.5,0,0.5),"lines"),
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

#conversion for size back to points of annotations
Size=0.3528*7

coral.mean<-coral %>% 
  dplyr::select(NRM_REGION, REEF,DEPTH,VISIT_NO,juv5.d.nf,CoralCover.score, Change.score.linear.mean,juv.score,ma.score,composition.score) %>%
  filter(!is.na(juv5.d.nf) & !is.na(Change.score.linear.mean) & !is.na(composition.score) ) %>%
  mutate(index=(CoralCover.score+ Change.score.linear.mean+ juv.score+ ma.score+ composition.score)/5) %>%
  group_by(NRM_REGION,REEF,DEPTH) %>%
  dplyr::select(-juv5.d.nf) %>%
  summarise_all(mean, na.rm=TRUE)

bom<- bom.month %>% filter(Year>2006) %>%
  dplyr::select(REEF,variableName, data) %>%
  group_by(REEF,variableName)%>%
  summarise_all(mean, na.rm=TRUE) %>%
  ungroup %>%
  group_by(REEF) %>%
  spread(variableName, data) %>%
  rename(chl=Chl_MIM_median) %>%
  rename(tss=Nap_MIM_median)

coral.17<-coral %>% filter(yr=="2017") %>%
  dplyr::select(NRM_REGION, REEF,DEPTH,composition.score) %>% left_join(bom) %>%
  mutate(composition.score=as.factor(composition.score))
  
scores<-coral.mean %>% left_join(bom)
score.env2<-scores %>% ungroup %>% filter(DEPTH=='2')
score.env5<-scores %>% ungroup %>% filter(DEPTH=='5')

##################
# Index
################

xlabs = as.expression(c((Chl~a~(ugL^{-1})),
                        (NAP~(mgL^{-1}))))
# 2m Chl

#B.pb<-gamlss(index ~ pb(chl), data = score.env2 %>% dplyr::select(chl,index) , family="BE")
i.c2<-gamlss(index ~ chl, data = score.env2 %>% dplyr::select(chl,index) , family="BE")
#B<-gamlss(index ~ 1, data = score.env2 %>% dplyr::select(chl,index) , family="BE")
# AICc(B.pb) #-23.2 # no support for curve
# AICc(i.c2) # -22.7 # support for linear
# AICc(B) # -18.6

# plot(B.l) # ok. 

pred.i.c2<-predict(i.c2, se=T, type="response")

summary(i.c2)
Rsq(i.c2)#0.184

lower.i.c2 <- as.vector(pred.i.c2$fit) - as.vector(pred.i.c2$se.fit*qt(0.975,df=df.residual(i.c2)))
upper.i.c2 <- as.vector(pred.i.c2$fit) + as.vector(pred.i.c2$se.fit*qt(0.975,df=df.residual(i.c2)))
fit.i.c2 <- as.vector(pred.i.c2$fit)

newdata.i.c2 <- data.frame(chl=score.env2$chl,
                      fit=fit.i.c2, 
                      lower=lower.i.c2, 
                      upper=upper.i.c2)

chl.2.index <- ggplot(newdata.i.c2, aes(y=fit, x=chl)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=chl), fill='grey',alpha=0.5)+
  geom_line(aes(y=fit, x=chl), color='black')+
  scale_x_continuous(xlabs[1])+
  scale_y_continuous('Index score')+
  theme_a()+
  theme(plot.title = element_text(hjust=0.5, size=rel(1),vjust=-0.5))+
  ggtitle("2 m depth")+
  geom_point(aes(y=index, x=chl), data=score.env2, size=1.1, color='darkgrey')+
 # geom_rug(aes(y=1,x=chl), sides='b')+
  annotate(geom='text', x=-Inf,y=0.95, label='a)', size=Size, hjust=-0.5, vjust=0)+
  annotate(geom='text', x=0.7,y=0.9, label='paste(italic(R)^2,\" = 0.184")',parse=TRUE, size=Size)

# 5m Chl

#B.pb<-gamlss(index ~ pb(chl), data = score.env5 %>% dplyr::select(chl,index) , family="BE")
i.c5<-gamlss(index ~ chl, data = score.env5 %>% dplyr::select(chl,index) , family="BE")
#B<-gamlss(index ~ 1, data = score.env5 %>% dplyr::select(chl,index) , family="BE")
#AICc(B.pb) #-49.3 # no support for curve
AICc(i.c5) # -48.1 # support for linear
#AICc(B) # -36.2

plot(i.c5) # ok. 

pred.i.c5<-predict(i.c5, se=T, type="response")

summary(i.c5)
Rsq(i.c5)#0.305

lower.i.c5 <- as.vector(pred.i.c5$fit) - as.vector(pred.i.c5$se.fit*qt(0.975,df=df.residual(i.c5)))
upper.i.c5 <- as.vector(pred.i.c5$fit) + as.vector(pred.i.c5$se.fit*qt(0.975,df=df.residual(i.c5)))
fit.i.c5 <- as.vector(pred.i.c5$fit)

newdata.i.c5 <- data.frame(chl=score.env5$chl,
                      fit=fit.i.c5, 
                      lower=lower.i.c5, 
                      upper=upper.i.c5)

chl.5.index<- ggplot(newdata.i.c5, aes(y=fit, x=chl)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=chl), fill='grey',alpha=0.5)+
  geom_line(aes(y=fit, x=chl), color='black')+
  scale_x_continuous(xlabs[1])+
  scale_y_continuous('Index score')+
  theme_a()+
  ggtitle("5 m depth")+
  geom_point(aes(y=index, x=chl), data=score.env5, size=1.1, colour='darkgrey')+
  #geom_rug(aes(y=1,x=chl), sides='b')+
  theme(plot.title = element_text(hjust=0.5, size=rel(1),vjust=-0.5))+
  
  annotate(geom='text', x=-Inf,y=0.95, label='b)', size=Size, hjust=-0.5, vjust=0)+
  annotate(geom='text', x=0.7,y=0.9, label='paste(italic(R)^2,\" = 0.305")',parse=TRUE, size=Size)

#TSS
#####
# 2m tss

B.pb<-gamlss(index ~ pb(tss), data = score.env2 %>% dplyr::select(tss,index) , family="BE")
B.l<-gamlss(index ~ tss, data = score.env2 %>% dplyr::select(tss,index) , family="BE")
B<-gamlss(index ~ 1, data = score.env2 %>% dplyr::select(tss,index) , family="BE")
AICc(B.pb) #-16.3 # no support for curve
AICc(B.l) # -16.3 # no support for linear
AICc(B) # -18.6
#####
# 5m tss

B.pb<-gamlss(index ~ pb(tss), data = score.env5 %>% dplyr::select(tss,index) , family="BE")
B.l<-gamlss(index ~ tss, data = score.env5 %>% dplyr::select(tss,index) , family="BE")
B<-gamlss(index ~ 1, data = score.env5 %>% dplyr::select(tss,index) , family="BE")
AICc(B.pb) #-35.3 # no support for curve
AICc(B.l) # -35.3 #  no support for linear
AICc(B) # -36.2

######################
# Coral Cover
#################

summary(score.env2$CoralCover.score) # no zeros or 1's
hist(score.env2$CoralCover.score,20) # even distribution

#2m Chl
######
B.pb<-gamlss(CoralCover.score ~ pb(chl), data = score.env2 %>% dplyr::select(chl,CoralCover.score) , family="BE")
B.l<-gamlss(CoralCover.score ~ chl, data = score.env2 %>% dplyr::select(chl,CoralCover.score) , family="BE")
B<-gamlss(CoralCover.score ~ 1, data = score.env2 %>% dplyr::select(chl,CoralCover.score) , family="BE")
AICc(B.pb) #-3.7 # no support for curve
AICc(B.l) # -3.2 # no support for linear
AICc(B) # -3.1


#####
# 5m Chl
#####
 B.pb<-gamlss(CoralCover.score ~ pb(chl), data = score.env5 %>% dplyr::select(chl,CoralCover.score) , family="BE")
 B.l<-gamlss(CoralCover.score ~ chl, data = score.env5 %>% dplyr::select(chl,CoralCover.score) , family="BE")
 B<-gamlss(CoralCover.score ~ 1, data = score.env5 %>% dplyr::select(chl,CoralCover.score) , family="BE")
 AICc(B.pb) #-10.5 # no support for curve
 AICc(B.l) # -10.5 # no support for linear
 AICc(B) # -9.7

#####
# 2m tss
#####
B.pb<-gamlss(CoralCover.score ~ pb(tss), data = score.env2 %>% dplyr::select(tss,CoralCover.score) , family="BE")
B.l<-gamlss(CoralCover.score ~ tss, data = score.env2 %>% dplyr::select(tss,CoralCover.score) , family="BE")
B<-gamlss(CoralCover.score ~ 1, data = score.env2 %>% dplyr::select(tss,CoralCover.score) , family="BE")
AICc(B.pb) #-1 # no support for curve
AICc(B.l) # -1 # no support for linear
AICc(B) # -3.1


# 5m tss
#####
B.pb<-gamlss(CoralCover.score ~ pb(tss), data = score.env5 %>% dplyr::select(tss,CoralCover.score) , family="BE")
B.l<-gamlss(CoralCover.score ~ tss, data = score.env5 %>% dplyr::select(tss,CoralCover.score) , family="BE")
B<-gamlss(CoralCover.score ~ 1, data = score.env5 %>% dplyr::select(tss,CoralCover.score) , family="BE")
AICc(B.pb) #-7.4 # no support for curve
AICc(B.l) # -7.4 # no support for linear
AICc(B) # -9.7


#####################
# Macro algae proportion
#################
hist(score.env2$ma.score,20) # includes 0 and 1
summary(score.env2$ma.score) # 

hist(score.env5$ma.score,20) # includes 0 and 1
summary(score.env5$ma.score) # 

# 2m Chl

#B.pb<-gamlss((ma.score*0.9998)+0.0001 ~ pb(chl), data = score.env2 %>% dplyr::select(chl,ma.score) , family="BE")
ma.c2<-gamlss((ma.score*0.9998)+0.0001 ~ chl, data = score.env2 %>% dplyr::select(chl,ma.score) , family="BE")
#B<-gamlss((ma.score*0.9998)+0.0001 ~ 1, data = score.env2 %>% dplyr::select(chl,ma.score) , family="BE")
#AICc(B.pb) #-87.1 # no support for curve
AICc(ma.c2) # -86.3 # support for linear
#AICc(B) # -81.4

plot(ma.c2) # ok. 

pred.ma.c2<-predict(ma.c2, se=T, type="response")

summary(ma.c2)
Rsq(ma.c2)#0.206

lower.ma.c2 <- as.vector(pred.ma.c2$fit) - as.vector(pred.ma.c2$se.fit*qt(0.975,df=df.residual(ma.c2)))
upper.ma.c2 <- as.vector(pred.ma.c2$fit) + as.vector(pred.ma.c2$se.fit*qt(0.975,df=df.residual(ma.c2)))
fit.ma.c2 <- as.vector(pred.ma.c2$fit)

newdata.ma.c2 <- data.frame(chl=score.env2$chl,
                      fit=fit.ma.c2, 
                      lower=lower.ma.c2, 
                      upper=upper.ma.c2)

chl.2.ma <- ggplot(newdata.ma.c2, aes(y=fit, x=chl)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=chl), fill='grey',alpha=0.5)+
  geom_line(aes(y=fit, x=chl), color='black')+
  scale_x_continuous(xlabs[1])+
  scale_y_continuous('Macroalgae score')+
  ggtitle("2 m depth")+
  theme_a()+
 # geom_rug(aes(y=1,x=chl), sides='b')+
  geom_point(aes(y=ma.score, x=chl), data=score.env2, size=1.1, colour='darkgrey')+
  theme(plot.title = element_text(hjust=0.5, size=rel(1),vjust=-0.5))+
  
  annotate(geom='text', x=1.0,y=0.95, label='c)', size=Size, hjust=0, vjust=0)+
  annotate(geom='text', x=0.7,y=0.82, label='paste(italic(R)^2,\" = 0.206")',parse=TRUE, size=Size)

# 5m Chl

#B.pb<-gamlss((ma.score*0.9998)+0.0001 ~ pb(chl), data = score.env5 %>% dplyr::select(chl,ma.score) , family="BE")
ma.c5<-gamlss((ma.score*0.9998)+0.0001 ~ chl, data = score.env5 %>% dplyr::select(chl,ma.score) , family="BE")
#B<-gamlss((ma.score*0.9998)+0.0001 ~ 1, data = score.env5 %>% dplyr::select(chl,ma.score) , family="BE")
#AICc(B.pb) #-57.2 # no support for curve
AICc(ma.c5) # -55.3 # support for linear
#AICc(B) # -45.6

plot(ma.c5) # ok. 

pred.ma.c5<-predict(ma.c5, se=T, type="response")

summary(ma.c5)
Rsq(ma.c5)#0.266

lower.ma.c5 <- as.vector(pred.ma.c5$fit) - as.vector(pred.ma.c5$se.fit*qt(0.975,df=df.residual(ma.c5)))
upper.ma.c5 <- as.vector(pred.ma.c5$fit) + as.vector(pred.ma.c5$se.fit*qt(0.975,df=df.residual(ma.c5)))
fit.ma.c5 <- as.vector(pred.ma.c5$fit)

newdata.ma.c5 <- data.frame(chl=score.env5$chl,
                            fit=fit.ma.c5, 
                            lower=lower.ma.c5, 
                            upper=upper.ma.c5)

chl.5.ma <- ggplot(newdata.ma.c5, aes(y=fit, x=chl)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=chl), fill='grey',alpha=0.5)+
  geom_line(aes(y=fit, x=chl), color='black')+
  scale_x_continuous(xlabs[1])+
  scale_y_continuous('Macroalgae score')+
  ggtitle("5 m depth")+
  theme_a()+
 # geom_rug(aes(y=1,x=chl), sides='b')+
  geom_point(aes(y=ma.score, x=chl), data=score.env5, size=1.1, colour='darkgrey')+
  theme(plot.title = element_text(hjust=0.5, size=rel(1),vjust=-0.5))+
  
  annotate(geom='text', x=1,y=0.95, label='d)', size=Size, hjust=0, vjust=0)+
  annotate(geom='text', x=0.7,y=0.85, label='paste(italic(R)^2,\" = 0.266")',parse=TRUE, size=Size)

# 2m tss
#####
B.pb<-gamlss((ma.score*0.9998)+0.0001 ~ pb(tss), data = score.env2 %>% dplyr::select(tss,ma.score) , family="BE")
B.l<-gamlss((ma.score*0.9998)+0.0001 ~ tss, data = score.env2 %>% dplyr::select(tss,ma.score) , family="BE")
B<-gamlss((ma.score*0.9998)+0.0001 ~ 1, data = score.env2 %>% dplyr::select(tss,ma.score) , family="BE")
AICc(B.pb) #-79.5 # no support for curve
AICc(B.l) # -79.5 # no support for linear
AICc(B) # -81.4
#####
# 5m tss
#####
B.pb<-gamlss((ma.score*0.9998)+0.0001 ~ pb(tss), data = score.env5 %>% dplyr::select(tss,ma.score) , family="BE")
B.l<-gamlss((ma.score*0.9998)+0.0001 ~ tss, data = score.env5 %>% dplyr::select(tss,ma.score) , family="BE")
B<-gamlss((ma.score*0.9998)+0.0001 ~ 1, data = score.env5 %>% dplyr::select(tss,ma.score) , family="BE")
AICc(B.pb) #-43.8 # no support for curve
AICc(B.l) # -43.8 # no support for linear
AICc(B) # -45.6

#####################
# Juveniles
#################

summary(score.env2$juv.score) # does not include 0 or 1
summary(score.env5$juv.score) # does not include 0 or 1

# 2m Chl
#####
B.pb<-gamlss(juv.score ~ pb(chl), data = score.env2 %>% dplyr::select(chl,juv.score) , family="BE")
B.l<-gamlss(juv.score ~ chl, data = score.env2 %>% dplyr::select(chl,juv.score) , family="BE")
B<-gamlss(juv.score ~ 1, data = score.env2 %>% dplyr::select(chl,juv.score) , family="BE")
AICc(B.pb) #-12.8 # no support for curve
AICc(B.l) # -12.8 # no support for linear
AICc(B) # -14.1

#####
# 5m Chl
#####
B.pb<-gamlss(juv.score ~ pb(chl), data = score.env5 %>% dplyr::select(chl,juv.score) , family="BE")
B.l<-gamlss(juv.score ~ chl, data = score.env5 %>% dplyr::select(chl,juv.score) , family="BE")
B<-gamlss(juv.score ~ 1, data = score.env5 %>% dplyr::select(chl,juv.score) , family="BE")
AICc(B.pb) #-1.9 # no support for curve
AICc(B.l) # -1.9 # no support for linear
AICc(B) # -4.1
#####
# 2m tss
#####
B.pb<-gamlss(juv.score ~ pb(tss), data = score.env2 %>% dplyr::select(tss,juv.score) , family="BE")
B.l<-gamlss(juv.score ~ tss, data = score.env2 %>% dplyr::select(tss,juv.score) , family="BE")
B<-gamlss(juv.score ~ 1, data = score.env2 %>% dplyr::select(tss,juv.score) , family="BE")
AICc(B.pb) #-11.7 # no support for curve
AICc(B.l) # -11.7 # no support for linear
AICc(B) # -14.1

#####
# 5m tss
#####
B.pb<-gamlss(juv.score ~ pb(tss), data = score.env5 %>% dplyr::select(tss,juv.score) , family="BE")
B.l<-gamlss(juv.score ~ tss, data = score.env5 %>% dplyr::select(tss,juv.score) , family="BE")
B<-gamlss(juv.score ~ 1, data = score.env5 %>% dplyr::select(tss,juv.score) , family="BE")
AICc(B.pb) #-2.1 # no support for curve
AICc(B.l) # -2 # no support for linear
AICc(B) # -4.1
#####################
# Change score
#################

summary(score.env2$Change.score.linear.mean) # no 0 or 1 
summary(score.env5$Change.score.linear.mean) # no 0 or 1

# 2m Chl
#####
B.pb<-gamlss(Change.score.linear.mean ~ pb(chl), data = score.env2 %>% dplyr::select(chl,Change.score.linear.mean) , family="BE")
B.l<-gamlss(Change.score.linear.mean ~ chl, data = score.env2 %>% dplyr::select(chl,Change.score.linear.mean) , family="BE")
B<-gamlss(Change.score.linear.mean ~ 1, data = score.env2 %>% dplyr::select(chl,Change.score.linear.mean) , family="BE")

AICc(B.pb) #-11.9 # no support for curve
AICc(B.l) # -11.9 # no support for linear
AICc(B) # -12.9
#####
#5m Chl
#####
B.pb<-gamlss(Change.score.linear.mean ~ pb(chl), data = score.env5 %>% dplyr::select(chl,Change.score.linear.mean), family="BE")
B.l<-gamlss(Change.score.linear.mean ~ chl, data = score.env5 %>% dplyr::select(chl,Change.score.linear.mean), family="BE")
B<-gamlss(Change.score.linear.mean ~ 1, data = score.env5 %>% dplyr::select(chl,Change.score.linear.mean), family="BE")

AICc(B.pb) #-14.4 # no support for curve
AICc(B.l) # -14.4# no support for linear
AICc(B) # -14.1
#####
# 2m tss
#####
B.pb<-gamlss(Change.score.linear.mean ~ pb(tss), data = score.env2 %>% dplyr::select(tss,Change.score.linear.mean) , family="BE")
B.l<-gamlss(Change.score.linear.mean ~ tss, data = score.env2 %>% dplyr::select(tss,Change.score.linear.mean) , family="BE")
B<-gamlss(Change.score.linear.mean ~ 1, data = score.env2 %>% dplyr::select(tss,Change.score.linear.mean) , family="BE")


AICc(B.pb) #-11.6 # no support for curve
AICc(B.l) # -11.6 # no support for linear
AICc(B) #-12.9
#####
# 5m tss
#####
#B.pb<-gamlss(Change.score.linear.mean ~ pb(tss), data = score.env5 %>% dplyr::select(tss,Change.score.linear.mean), family="BE")
ch.t5<-gamlss(Change.score.linear.mean ~ tss, data = score.env5 %>% dplyr::select(tss,Change.score.linear.mean), family="BE")
#B<-gamlss(Change.score.linear.mean ~ 1, data = score.env5 %>% dplyr::select(tss,Change.score.linear.mean), family="BE")

#AICc(B.pb) #-16.3 # no support for curve
AICc(ch.t5) # -16.2 #  support for linear
#AICc(B) #-14.1

plot(ch.t5) # ok. 

pred.ch.t5<-predict(ch.t5, se=T, type="response")

summary(ch.t5)
Rsq(ch.t5)#0.108

lower.ch.t5 <- as.vector(pred.ch.t5$fit) - as.vector(pred.ch.t5$se.fit*qt(0.975,df=df.residual(ch.t5)))
upper.ch.t5 <- as.vector(pred.ch.t5$fit) + as.vector(pred.ch.t5$se.fit*qt(0.975,df=df.residual(ch.t5)))
fit.ch.t5 <- as.vector(pred.ch.t5$fit)

newdata.ch.t5 <- data.frame(tss=score.env5$tss,
                            fit=fit.ch.t5, 
                            lower=lower.ch.t5, 
                            upper=upper.ch.t5)

tss.5.ch <- ggplot(newdata.ch.t5, aes(y=fit, x=tss)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=tss), fill='grey',alpha=0.5)+
  geom_line(aes(y=fit, x=tss), color='black')+
  scale_x_continuous(xlabs[2])+
  scale_y_continuous('Change score')+
  ggtitle("5 m depth")+
  theme_a()+
  geom_point(aes(y=Change.score.linear.mean, x=tss), data=score.env5, size=1.1, colour='darkgrey')+
  theme(plot.title = element_text(hjust=0.5, size=rel(1),vjust=-0.5))+
  
  annotate(geom='text', x=-Inf,y=0.95, label='e)', size=Size, hjust=-0.5, vjust=0)+
  annotate(geom='text', x=1.3,y=0.81, label='paste(italic(R)^2,\" = 0.108")',parse=TRUE, size=Size)


##########################
# Composition score
##########################
library(brms)
library(broom)

#It makes more sense to compare only the long-term aspect of this variable, so score in 2017 rather than the average
summary(coral.17$composition.score) # 2 NA's

#2m chl
#####
mod = polr(composition.score ~ chl, data=coral.17 %>% filter(!is.na(composition.score) & DEPTH=='2'))
mod1 = polr(composition.score ~ 1, data=coral.17 %>% filter(!is.na(composition.score) & DEPTH=='2'))
AICc(mod)#69.7 no support for Chl
AICc(mod1)#67.6
#####
#5m chl
#####
mod.ch5 = polr(composition.score ~ chl, data=coral.17 %>% filter(!is.na(composition.score) & DEPTH=='5'))
mod1 = polr(composition.score ~ 1, data=coral.17 %>% filter(!is.na(composition.score) & DEPTH=='5'))
AICc(mod.ch5)#84 support for Chl
AICc(mod1)#86.8
#cox & snell pseudo R2
N <- nrow(coral.17 %>% filter(!is.na(composition.score) & DEPTH=='5'))
m.ll <- logLik(mod.ch5)[1]
n.ll <- logLik(mod1)[1]
cs.R2 <- 1 - exp((-2/N)*(m.ll - n.ll)) #0.127

library(effects)
plot(allEffects(mod.ch5))
#grid(Effects(mod.ch5))
pred<-predict(mod.ch5, type='probs', se=T)
predict(mod.ch5, type='probs', se=TRUE)

#dat=as.data.frame(Effect('chl',mod.ch5, xlevels=list(chl=seq(min(score.env5$chl),max(score.env5$chl),len=100))))
dat=as.data.frame(Effect('chl',mod.ch5, xlevels=list(chl=score.env5$chl)))
 head(dat)
 dat= dat%>% dplyr::select(chl,prob.X0,se.prob.X0) #%>%
 lower <- as.vector(dat$prob.X0 - dat$se.prob.X0)
 upper <- as.vector(dat$prob.X0 + dat$se.prob.X0)
 fit <- as.vector(dat$prob.X0)
 
 newdata <- data.frame(chl=dat$chl,
                       fit=fit, 
                       lower=lower, 
                       upper=upper)
 
 chl.5.comp.p <- ggplot(newdata, aes(y=fit, x=chl)) +
   geom_ribbon(aes(ymin=lower, ymax=upper, x=chl), fill='grey',alpha=0.5)+
   geom_line(aes(y=fit, x=chl), color='black')+
   scale_x_continuous(xlabs[1])+
   scale_y_continuous('Probability Composition score=0')+
   theme_classic(base_size=9)+
   ggtitle("5 m depth")+
   geom_rug(aes(y=1,x=chl), sides='b')+
   theme_a()+
   theme(plot.title = element_text(hjust=0.5, size=rel(1), vjust=-0.5))+
   
   annotate(geom='text', x=-Inf,y=0.95, label='f)', size=Size, hjust=-0.5, vjust=0)+
   annotate(geom='text', x=0.6,y=0.9, label='paste(italic(R)^2,\" = 0.127")',parse=TRUE, size=Size)


#2m tss

#####
mod = polr(composition.score ~ tss, data=coral.17 %>% filter(!is.na(composition.score) & DEPTH=='2'))
mod1 = polr(composition.score ~ 1, data=coral.17 %>% filter(!is.na(composition.score) & DEPTH=='2'))
AICc(mod)#70 no support for tss
AICc(mod1)#67.6
#####
#5m tss
#####
mod = polr(composition.score ~ tss, data=coral.17 %>% filter(!is.na(composition.score) & DEPTH=='5'))
mod1 = polr(composition.score ~ 1, data=coral.17 %>% filter(!is.na(composition.score) & DEPTH=='5'))
AICc(mod)#88.9 no support for tss
AICc(mod1)#86.8


############# FIGURE 1****************************************************************
ggsave('output/Figure1 1column.png', width=90, height=150, units="mm", dpi=600,
       plot=grid.arrange(chl.2.index,chl.5.index,
                         chl.2.ma,chl.5.ma,
                         tss.5.ch, chl.5.comp.p, nrow=3))

ggsave('output/Figure1 1column.pdf', width=90, height=150, units="mm", dpi=600,
       plot=grid.arrange(chl.2.index,chl.5.index,
                         chl.2.ma,chl.5.ma,
                         tss.5.ch, chl.5.comp.p, nrow=3))

ggsave('output/Figure1 1column.jpg', width=90, height=150, units="mm", dpi=600,
       plot=grid.arrange(chl.2.index,chl.5.index,
                         chl.2.ma,chl.5.ma,
                         tss.5.ch, chl.5.comp.p, nrow=3))
####***********************************************************************************

#####################
# Juveniles proportion AC v TURB
#################
jad.reef<-read.csv(file="dataRepository/jad.reef.csv")

jad<-jad.reef %>% ungroup %>%
  group_by(REEF,DEPTH) %>%
  summarise(propAC=mean(propAC))

jad2<-jad %>% ungroup %>% filter(DEPTH==2) %>% dplyr::select(-DEPTH)
jad5<-jad %>% ungroup %>% filter(DEPTH==5) %>% dplyr::select(-DEPTH)

score.env2j<-score.env2 %>% left_join(jad2) %>% filter(!is.na(propAC))
score.env5j<-score.env5 %>% left_join(jad5) %>% filter(!is.na(propAC))
summary(score.env2j$propAC) # includes 1
summary(score.env5j$propAC) 

# 2m Chl
#####
B.pb<-gamlss(propAC-0.0001 ~ pb(chl), data = score.env2j %>% dplyr::select(chl,propAC) , family="BE")
pAC.c2<-gamlss(propAC-0.0001 ~ chl, data = score.env2j %>% dplyr::select(chl,propAC) , family="BE")
B<-gamlss(propAC-0.0001 ~ 1, data = score.env2j %>% dplyr::select(chl,propAC) , family="BE")
AICc(B.pb) # -53.25 # no support for curve 
AICc(pAC.c2) # -53.25 # support for linear
AICc(B) # -47.3

pred.pAC.c2<-predict(pAC.c2, se=T, type="response")

summary(pAC.c2)
Rsq(pAC.c2)#0.251

lower.pAC.c2 <- as.vector(pred.pAC.c2$fit) - as.vector(pred.pAC.c2$se.fit*qt(0.975,df=df.residual(pAC.c2)))
upper.pAC.c2 <- as.vector(pred.pAC.c2$fit) + as.vector(pred.pAC.c2$se.fit*qt(0.975,df=df.residual(pAC.c2)))
fit.pAC.c2 <- as.vector(pred.pAC.c2$fit)

newdata.pAC.c2 <- data.frame(chl=score.env2j$chl,
                            fit=fit.pAC.c2, 
                            lower=lower.pAC.c2, 
                            upper=upper.pAC.c2)

chl.2.pAC <- ggplot(newdata.pAC.c2, aes(y=fit, x=chl)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=chl), fill='grey',alpha=0.5)+
  geom_line(aes(y=fit, x=chl), color='black')+
  geom_point(aes(y=propAC, x=chl), data=score.env2j, size=1.1, colour='darkgrey')+
  scale_x_continuous(xlabs[1])+
  scale_y_continuous('Proportion Acroporidae')+
  ggtitle("2 m depth")+
  theme_a()+
  theme(plot.title = element_text(hjust=0.5, size=rel(1),vjust=-0.5))+
  
  annotate(geom='text', x=1,y=0.95, label='a)', size=Size, hjust=0, vjust=0)+
  annotate(geom='text', x=0.55,y=0.35, label='paste(italic(R)^2,\" = 0.251")',parse=TRUE, size=Size)

#####
# 5m Chl
#####
B.pb<-gamlss(propAC-0.0001 ~ pb(chl), data = score.env5j %>% dplyr::select(chl,propAC) , family="BE")
pAC.c5<-gamlss(propAC-0.0001 ~ chl, data = score.env5j %>% dplyr::select(chl,propAC) , family="BE")
B<-gamlss(propAC-0.0001 ~ 1, data = score.env5j %>% dplyr::select(chl,propAC) , family="BE")
AICc(B.pb) #-30.9 # no support for curve
AICc(pAC.c5) # -30.9# support for linear
AICc(B) # -8.7

pred.pAC.c5<-predict(pAC.c5, se=T, type="response")

summary(pAC.c5)
Rsq(pAC.c5)#0.486

lower.pAC.c5 <- as.vector(pred.pAC.c5$fit) - as.vector(pred.pAC.c5$se.fit*qt(0.975,df=df.residual(pAC.c5)))
upper.pAC.c5 <- as.vector(pred.pAC.c5$fit) + as.vector(pred.pAC.c5$se.fit*qt(0.975,df=df.residual(pAC.c5)))
fit.pAC.c5 <- as.vector(pred.pAC.c5$fit)

newdata.pAC.c5 <- data.frame(chl=score.env5j$chl,
                             fit=fit.pAC.c5, 
                             lower=lower.pAC.c5, 
                             upper=upper.pAC.c5)

chl.5.pAC <- ggplot(newdata.pAC.c5, aes(y=fit, x=chl)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=chl), fill='grey',alpha=0.5)+
  geom_line(aes(y=fit, x=chl), color='black')+
  geom_point(aes(y=propAC, x=chl), data=score.env5j, size=1.1, colour='darkgrey')+
  scale_x_continuous(xlabs[1])+
  scale_y_continuous('Proportion Acroporidae')+
  ggtitle("5 m depth")+
  theme_a()+
  theme(plot.title = element_text(hjust=0.5, size=rel(1),vjust=-0.5))+
  
  annotate(geom='text', x=1,y=0.95, label='c)', size=Size, hjust=0, vjust=0)+
  annotate(geom='text', x=0.55,y=0.25, label='paste(italic(R)^2,\" = 0.486")',parse=TRUE, size=Size)
#####
# 2m tss
#####
B.pb<-gamlss(propAC-0.0001 ~ pb(tss), data = score.env2j %>% dplyr::select(tss,propAC) , family="BE")
pAC.t2<-gamlss(propAC-0.0001 ~ tss, data = score.env2j %>% dplyr::select(tss,propAC) , family="BE")
B<-gamlss(propAC-0.0001 ~ 1, data = score.env2j %>% dplyr::select(tss,propAC) , family="BE")
AICc(B.pb) #-51.7 # no support for curve
AICc(pAC.t2) # -51.7 # support for linear
AICc(B) # -47.4

plot(pAC.t2) # ok. 

pred.pAC.t2<-predict(pAC.t2, se=T, type="response")

summary(pAC.t2)
Rsq(pAC.t2)#0.210

lower.pAC.t2 <- as.vector(pred.pAC.t2$fit) - as.vector(pred.pAC.t2$se.fit*qt(0.975,df=df.residual(pAC.t2)))
upper.pAC.t2 <- as.vector(pred.pAC.t2$fit) + as.vector(pred.pAC.t2$se.fit*qt(0.975,df=df.residual(pAC.t2)))
fit.pAC.t2 <- as.vector(pred.pAC.t2$fit)

newdata.pAC.t2 <- data.frame(tss=score.env2j$tss,
                             fit=fit.pAC.t2, 
                             lower=lower.pAC.t2, 
                             upper=upper.pAC.t2)

tss.2.pAC <- ggplot(newdata.pAC.t2, aes(y=fit, x=tss)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=tss), fill='grey',alpha=0.5)+
  geom_line(aes(y=fit, x=tss), color='black')+
  geom_point(aes(y=propAC, x=tss), data=score.env2j, size=1.1, colour='darkgrey')+
  scale_x_continuous(xlabs[2])+
  scale_y_continuous('Proportion Acroporidae')+
  ggtitle("2 m depth")+
  theme_classic(base_size=9)+
  theme(plot.title = element_text(hjust=0.5, size=rel(1),vjust=-0.5))+
  
  annotate(geom='text', x=3,y=0.95, label='b)', size=Size, hjust=-0.5, vjust=0)+
  annotate(geom='text', x=1.5,y=0.35, label='paste(italic(R)^2,\" = 0.210")',parse=TRUE, size=Size)

#####
# 5m tss
#####
B.pb<-gamlss(propAC-0.0001 ~ pb(tss), data = score.env5j %>% dplyr::select(tss,propAC) , family="BE")
pAC.t5<-gamlss(propAC-0.0001 ~ tss, data = score.env5j %>% dplyr::select(tss,propAC) , family="BE")
B<-gamlss(propAC-0.0001 ~ 1, data = score.env5j %>% dplyr::select(tss,propAC) , family="BE")
AICc(B.pb) #-14.8 # no support for curve
AICc(pAC.t5) # -14.8 # no support for linear
AICc(B) # -8.7

plot(pAC.t5) # ok. 

pred.pAC.t5<-predict(pAC.t5, se=T, type="response")

summary(pAC.t5)
Rsq(pAC.t5)#0.203

lower.pAC.t5 <- as.vector(pred.pAC.t5$fit) - as.vector(pred.pAC.t5$se.fit*qt(0.975,df=df.residual(pAC.t5)))
upper.pAC.t5 <- as.vector(pred.pAC.t5$fit) + as.vector(pred.pAC.t5$se.fit*qt(0.975,df=df.residual(pAC.t5)))
fit.pAC.t5 <- as.vector(pred.pAC.t5$fit)

newdata.pAC.t5 <- data.frame(tss=score.env5j$tss,
                            fit=fit.pAC.t5, 
                            lower=lower.pAC.t5, 
                            upper=upper.pAC.t5)

tss.5.pAC <- ggplot(newdata.pAC.t5, aes(y=fit, x=tss)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, x=tss), fill='grey',alpha=0.5)+
  geom_line(aes(y=fit, x=tss), color='black')+
  geom_point(aes(y=propAC, x=tss), data=score.env5j, size=1.1, colour='darkgrey')+
  scale_x_continuous(xlabs[2])+
  scale_y_continuous('Proportion Acroporidae')+
  ggtitle("5 m depth")+
  theme_a()+
  theme(plot.title = element_text(hjust=0.5, size=rel(1),vjust=-0.5))+
  
  annotate(geom='text', x=2.3,y=0.95, label='d)', size=Size, hjust=0, vjust=0)+
  annotate(geom='text', x=1.1,y=0.25, label='paste(italic(R)^2,\" = 0.203")',parse=TRUE, size=Size)

############# FIGURE S1****************************************************************
ggsave('output/FigureS1 1column.png', width=90, height=100, units="mm", dpi=600,
       plot=grid.arrange(chl.2.pAC,tss.2.pAC,
                         chl.5.pAC,tss.5.pAC,
                         nrow=2))

ggsave('output/FigureS1 1column.pdf', width=90, height=100, units="mm", dpi=600,
       plot=grid.arrange(chl.2.pAC,tss.2.pAC,
                         chl.5.pAC,tss.5.pAC,
                         nrow=2))

ggsave('output/FigureS1 1column.jpg', width=90, height=100, units="mm", dpi=600,
       plot=grid.arrange(chl.2.pAC,tss.2.pAC,
                         chl.5.pAC,tss.5.pAC,
                         nrow=2))
####***********************************************************************************
