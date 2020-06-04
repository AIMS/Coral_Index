
##
## The aggregation schedule

 ##      +------------------------+      
 ##      | Reef/Depth level Scores| 
 ##      |   e.g. Coral Cover 2m  |
 ##      +------------------------+
 ##                |
 ##                |---------------------------------------------------------------------------+
 ##                |                                                                           |
 ##                V                                                                           V
 ##   +------------------------------+     +--------------------------------+       +-----------------------+      +-----------------+
 ##   | Sub-region/Depth level Scores|---->| Sub-region/ depth level Index  |       | subregion level scores| ---> | subregion index |
 ##   +------------------------------+     +--------------------------------+       +-----------------------+      +-----------------+
 ##                |                                                                           |
 ##                V                                                                           V
 ##   +----------------------------+       +-------------------------+              +---------------------+       +-------------------+
 ##   | Region/Depth level Scores  |------>| Region/depth level Index|              | Region level scores | ----> | Region level index
 ##   +----------------------------+       +-------------------------+              +---------------------+       +-------------------+

library(scales)
library(tidyverse)
set.seed(123)
Size <- 1000

load(file='output/coral.RData')
coral <- coral %>% rename(Region=NRM_REGION, Year=yr) %>%
  mutate(subregion=factor(subregion, levels=c('Daintree','Johnstone','Tully','Burdekin','Proserpine','Fitzroy'),
                          labels=c('Barron Daintree','Johnstone Russell Mulgrave','Tully Herbert','Burdekin','Mackay Whitsunday','Fitzroy')))
  
# a function to estimate confidence intervals from the following boot strapped distributions, based on origional observation size
conf <- function(x) {
   tt<-table(x)
  l <- sum(tt/min(tt))
  s <- NULL
  for (i in 1:1000) {
    s <- c(s, mean(sample(x,size=l, replace=TRUE), na.rm=TRUE))
  }
  q<-quantile(s, na.rm=TRUE, p=c(0.025,0.975))
  c(q[1],q[2])
}

#functions to apply reportcard grade categories
boot_signal <-function(Var) {
  Confidence=rescale(sqrt(Var), from=c(0,sqrt(var(c(rep(0,5000),rep(1,5000))))), to=c(1,0))
  Signal = ifelse(Confidence>=0.8,5,ifelse(Confidence>=0.6,4,ifelse(Confidence>=0.4,3,ifelse(Confidence>=0.2,2,1))))
  Signal
}

MMP_generateGrades <- function(x) {
  ifelse(is.na(x),'NA',ifelse(x>=0.8, 'A', ifelse(x>=0.6, 'B', ifelse(x>=0.4, 'C',  ifelse(x>=0.2, 'D', 'E')))))
}

#a function to summarise botstraped distributions
boot_sum <- function(x) {
  require(scales)
  Mean=mean(x$Boot, na.rm=TRUE)
  Var=var(x$Boot, na.rm=TRUE)
  Confidence=rescale(sqrt(Var), from=c(0,sqrt(var(c(rep(0,5000),rep(1,5000))))), to=c(1,0))
  Signal = ifelse(Confidence>=0.8,5,ifelse(Confidence>=0.6,4,ifelse(Confidence>=0.4,3,ifelse(Confidence>=0.2,2,1))))
  Grade=MMP_generateGrades(Mean)
  data.frame(Mean=Mean, Var=Var, Signal=Signal,Grade=Grade)
}

## Bootstrapp Aggregations---------------------------------------------------------------------------------------------------------------------------------------------------
## Prepare for bootstrapping aggregation
coral.melt <- coral %>% select(P_CODE,Region,subregion,REEF,DEPTH,Year,Date,Change.score.linear.mean,ma.score,CoralCover.score,juv.score,composition.score) %>%
    rename('Macroalgal cover'=ma.score, 'Coral cover'=CoralCover.score,'Juvenile density'=juv.score, 'Coral change'=Change.score.linear.mean, 'Coral composition'=composition.score) %>%
  gather(key=Score,value=Index,-P_CODE,-Region,-subregion,-REEF,-DEPTH,-Year,-Date) %>%
  filter(!is.na(Index))
       

## subregion level===============================================================================================================================
coral.boot.subregion<-coral.melt %>% select(Region,subregion,Year,Score,Index)  %>% 
  ungroup %>% mutate(Boot=Index) %>%
  group_by(Region,subregion,Year,Score)  %>% sample_n(size=Size, replace=TRUE) 

##   indicator cores  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
boot.subregion.score = coral.boot.subregion %>% 
  ungroup %>% 
  group_by(Region,subregion,Year,Score)  %>% sample_n(size=Size, replace=TRUE) 
boot.subregion.score.sum = boot.subregion.score  %>% summarize(Mean=mean(Boot, na.rm=TRUE), Var=var(Boot,na.rm=TRUE), Signal=boot_signal(var(Boot,na.rm=TRUE))) %>% mutate(Grade=MMP_generateGrades(Mean))

##   Coral Index +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
boot.subregion.metric.sum = (boot.subregion.metric = boot.subregion.score %>% ungroup %>% select(-Score) %>% 
                                     group_by(Region,subregion,Year) %>%
                                     do({
                                       Full<-.
                                       Sum=Full %>% select(-Boot) %>% distinct() %>% cbind(boot_sum(Full))
                                       Full
                                     })) %>% summarize(Mean=mean(Boot, na.rm=TRUE), Var=var(Boot,na.rm=TRUE),lower=conf(Boot)[1],upper=conf(Boot)[2], Signal=boot_signal(var(Boot,na.rm=TRUE))) %>% mutate(Grade=MMP_generateGrades(Mean))

## Region level===============================================================================================================================
##   indicator scores  +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
boot.region.score.sum = (boot.region.score = boot.subregion.score %>% ungroup %>%  
                                 group_by(Region,Year,Score) %>%
                                 do({
                                   Full<-.
                                   Sum=Full %>% select(-Boot) %>% distinct() %>% cbind(boot_sum(Full))
                                   Full
                                 })) %>% summarize(Mean=mean(Boot, na.rm=TRUE), Var=var(Boot,na.rm=TRUE),lower=conf(Boot)[1],upper=conf(Boot)[2], Signal=boot_signal(var(Boot,na.rm=TRUE))) %>% mutate(Grade=MMP_generateGrades(Mean))
save(boot.region.score.sum, file='output/boot.region.score.sum.RData')
#save(boot.region.score, file='boot.region.score.RData')

##   Coral Index +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
boot.region.metric.sum = (boot.region.metric= boot.region.score %>% ungroup %>% select(-Score) %>%
                                  group_by(Region,Year) %>%
                                  do({
                                    Full<-.
                                    Sum=Full %>% select(-Boot) %>% distinct() %>% cbind(boot_sum(Full))
                                    Full
                                  })) %>% summarize(Mean=mean(Boot, na.rm=TRUE), Var=var(Boot,na.rm=TRUE),lower=conf(Boot)[1],upper=conf(Boot)[2], Signal=boot_signal(var(Boot,na.rm=TRUE))) %>% mutate(Grade=MMP_generateGrades(Mean))
save(boot.region.metric.sum, file='output/boot.region.metric.sum.RData')
#save(boot.region.metric, file='boot.region.metric.RData')

