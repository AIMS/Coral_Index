library(zoo)
library(tidyverse)
library(lubridate)
library(gtable)
library(cowplot)
####################################################
## Water temperature logger data (daily averages) ##
####################################################

waterTemp <- read.csv(paste0('dataRepository/waterTemp.csv'))
waterTemp$Date <- as.Date(waterTemp $DAY, format='%d/%m/%Y')

# load('data/names.lookup.RData')
# write.csv(names.lookup, file='dataRepository/names.lookup.csv')
names.lookup<-read.csv('dataRepository/names.lookup.csv')
temp.locations<-read.csv(file='dataRepository/temperature.lookup.csv')

water.temp.raw<-waterTemp %>% 
  filter(!is.na(LEVEL1_AVG)) %>%
  left_join(temp.locations) %>%
  left_join(names.lookup %>% select(REEF,NRM_REGION) %>% unique())

#######################################################################################################################
## Climatology for DHW - mean maximum-month temp at the LOCATION eg. PANSL1, PANFL1                                   #
## Data upto and including 2005-2015 used.                                                                            #
## For Keppels, 2006 excluded as bleaching occured                                                                    #
## For Location level summer max, where mean based on < 4 years of data, value from nearest neighbouring reef used    #
#######################################################################################################################

##### LOCATION level mean maximum-month temperature for DHW estimate

## water.temp.raw has daily average temperature, some timeseires require aggrecation of same reef locations
location.temp<-water.temp.raw %>%
  mutate(month=month(Date),
         year=year(Date),
         kep=ifelse(NRM_REGION=='Fitzroy',1,0),
         LOCATION_NAME=recode(LOCATION_NAME, CBFL2='CBFL4',
                              CBSL2='CBSL4',
                              GBFL3='GBMMPFL',
                              GBSL3='GBMMPSL',
                              HALFL1='HUMPFL1',
                              HALSL1='HUMPSL1',
                              KEPFL1='NKEPPFL1',
                              KEPSL1='NKEPPSL1')) %>%
  group_by(NRM_REGION, kep, year, month, LOCATION_NAME, Date) %>%
  summarise(Temp=mean(LEVEL1_AVG)) %>%
  filter(year>2004) %>%
  ungroup

# in all cases max temp was in Jan or Feb so base location level max on the max of these two months
climate<-location.temp %>%
  filter(year<2016 & month %in% seq(1,2,1) & !is.na(T)) %>%
  filter(kep=='0' | !year=='2006') 

#monthly means, excluding months with fewer than 15 observations, then years with excluded months

loc.max.month.mean<- climate %>%
  group_by(LOCATION_NAME, year, month) %>%
  summarise(Temp=mean(Temp, na.rm=T),
            obs=n()) %>%
  ungroup %>%
  filter(obs>15) %>%
  group_by(LOCATION_NAME, year) %>%
  summarise(Temp.max.month.yr=max(Temp),
            obs=n()) %>%
  filter(obs=='2') %>% 
  ungroup

loc.mean.max.month=loc.max.month.mean %>%
  group_by(LOCATION_NAME) %>%
  summarise(Temp.max=mean(Temp.max.month.yr),
            obs=n()) %>%
  ungroup %>%
  filter(obs>3) 
###----- apply nearest neighbour climatology to locations with insufficent time-series to estimate their own climatology
#full list of locations, merge to loc.mean.max.month
loc.summer.max<-data.frame(LOCATION_NAME=unique(location.temp$LOCATION_NAME)) %>% left_join(loc.mean.max.month) 

surrogates1<-loc.summer.max %>% 
  filter(LOCATION_NAME %in% c('BARRSL1','DUNKFL1','DUNKSL1','HAVSL1','HOOKSL1','LELLSL1','PANSL1','SHTANFL1','NEFL1','PIFL1')) %>%
  mutate(LOCATION_NAME=recode(LOCATION_NAME,
                              'BARRSL1'='BARRFL1',
                              'DUNKFL1'='BDRAFL1',
                              'DUNKSL1'='BDRASL1',
                              'HAVSL1'='HAVFL1',
                              'HOOKSL1'='HOOKFL1',
                              'LELLSL1'='LELLFL1',
                              'PANSL1'='PANSL2',
                              'SHTANFL1'='SHTANSL1',
                              'NEFL1'='NESL1',
                              'PIFL1'='PISL1'))

surrogates2<-loc.summer.max %>% 
  filter(LOCATION_NAME =='HAVSL1') %>%
  mutate(LOCATION_NAME=recode(LOCATION_NAME,
                              'HAVSL1'='HAVSL2'))

surrogate.summer.max=rbind(surrogates1,surrogates2)

summer.max<-loc.mean.max.month %>% 
  rbind(surrogate.summer.max) %>%
  select(-obs)

# DHW from max.summer - this does not include the +1 adjustment used by NOAA, model estimate shrikage issue??
 DHW.logger<- location.temp %>% left_join(summer.max) %>%
   filter(month %in% c(12,1,2,3) & year<2018) %>%
  mutate(Year = factor( ifelse(month=='12',year+1,year)),
         heating.1deg=ifelse(Temp>(Temp.max+1), Temp-(Temp.max+1),0),
         heating.abs=ifelse(Temp>(Temp.max), Temp-(Temp.max),0)) %>%
group_by(NRM_REGION,Year, LOCATION_NAME) %>%
   summarise(DHW=sum(heating.abs/7),
             DHW.1deg=sum(heating.1deg/7),  # note this applies to a 4 month rather than 12 week period as per DHW NOAA.
             n.days=n()) %>% 
   ungroup %>%
   filter(n.days>110)   #ensure reasonable sample over summer
   
 # plot S2
 f.temp<- DHW.logger %>% filter(NRM_REGION=="Fitzroy") %>%
   ggplot(aes(y=DHW, x=Year)) + 
   #ggplot(aes(y=DHW.1deg, x=Year)) + 
   geom_hline(yintercept=4, linetype="dashed") +
   geom_boxplot() +
   scale_y_continuous('Degree heating weeks', lim=c(0,20)) +
   theme_classic()+
   #annotate(geom='text', x=-Inf,y= 9, label='a)', size=0.3528*11, hjust=-1, vjust=0)+
   ggtitle("Fitzroy")+
   theme(strip.background=element_blank(),
         strip.text=element_blank(),
         plot.margin=unit(c(0.5,0.5,1,1),"lines"),
         plot.title = element_text(hjust=0.5, size=rel(1)),
         panel.spacing=unit(c(0),"lines"),
         #axis.title.y=element_blank(),
         axis.title.x=element_blank(),
         axis.text.x=element_text(size=10,angle = 90, hjust = 1),
         axis.text.y=element_text(size=10)
   )
 wet.temp<- DHW.logger %>% filter(NRM_REGION=="Wet Tropics") %>%
   ggplot(aes(y=DHW, x=Year)) + 
   #ggplot(aes(y=DHW.1deg, x=Year)) + 
   geom_hline(yintercept=4, linetype="dashed") +
   geom_boxplot() +
   scale_y_continuous('Degree heating weeks', lim=c(0,20)) +
   theme_classic()+
   #annotate(geom='text', x=-Inf,y= 9, label='a)', size=0.3528*11, hjust=-1, vjust=0)+
   ggtitle("Wet Tropics")+
   theme(strip.background=element_blank(),
         strip.text=element_blank(),
         plot.margin=unit(c(0.5,0.5,1,1),"lines"),
         plot.title = element_text(hjust=0.5, size=rel(1)),
         panel.spacing=unit(c(0),"lines"),
         axis.title.x=element_blank(),
         axis.text.x=element_text(size=10,angle = 90, hjust = 1),
         axis.text.y=element_text(size=10)
   )
 b.temp<- DHW.logger %>% filter(NRM_REGION=="Burdekin") %>%
   ggplot(aes(y=DHW, x=Year)) + 
   #ggplot(aes(y=DHW.1deg, x=Year)) + 
   geom_hline(yintercept=4, linetype="dashed") +
   geom_boxplot() +
   scale_y_continuous('Degree heating weeks', lim=c(0,20)) +
   theme_classic()+
   #annotate(geom='text', x=-Inf,y= 9, label='a)', size=0.3528*11, hjust=-1, vjust=0)+
   ggtitle("Burdekin")+
   theme(strip.background=element_blank(),
         strip.text=element_blank(),
         plot.margin=unit(c(0.5,0.5,1,1),"lines"),
         plot.title = element_text(hjust=0.5, size=rel(1)),
         panel.spacing=unit(c(0),"lines"),
         axis.title.x=element_blank(),
         axis.text.x=element_text(size=10,angle = 90, hjust = 1),
         axis.text.y=element_text(size=10)
   )
 wh.temp<- DHW.logger %>% filter(NRM_REGION=="Mackay Whitsunday") %>%
   ggplot(aes(y=DHW, x=Year)) + 
  # ggplot(aes(y=DHW.1deg, x=Year)) + 
   geom_hline(yintercept=4, linetype="dashed") +
   geom_boxplot() +
   scale_y_continuous('Degree heating weeks', lim=c(0,20)) +
   theme_classic()+
   #annotate(geom='text', x=-Inf,y= 9, label='a)', size=0.3528*11, hjust=-1, vjust=0)+
   ggtitle("Mackay Whitsunday")+
   theme(strip.background=element_blank(),
         strip.text=element_blank(),
         plot.margin=unit(c(0.5,0.5,1,1),"lines"),
         plot.title = element_text(hjust=0.5, size=rel(1)),
         panel.spacing=unit(c(0),"lines"),
         axis.title.x=element_blank(),
         axis.text.x=element_text(size=10,angle = 90, hjust = 1),
         axis.text.y=element_text(size=10)
   )
 

 png('output/FigureS2.png', width=7, height=7, units="in", res=600)
 plot_grid(wet.temp,b.temp, wh.temp,f.temp, nrow=2)
 dev.off()
 
 pdf('output/FigureS2.pdf', width=7, height=7)
 plot_grid(wet.temp,b.temp, wh.temp,f.temp, nrow=2)
 dev.off()