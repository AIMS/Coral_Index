
library(coda)
library(scales)
library(tidyverse)
library(lubridate)

##-----Load observed indicator data
all.reef<-read.csv("dataRepository/all.reef.csv")

# Gompertz model parameter estimates for Acroporidae (Acr) on all other corals (oth) at 2m and 5m depths
gomp.oth.2<-read.csv( file="dataRepository/gomp.oth.2.csv")
gomp.oth.5<-read.csv( file="dataRepository/gomp.oth.5.csv")
gomp.acr.2<-read.csv( file="dataRepository/gomp.acr.2.csv")
gomp.acr.5<-read.csv( file="dataRepository/gomp.acr.5.csv")

#reef specific thresholds for macroalgae, based on distribution of MA along gradients of Chl a and TSS concentration
MA.limits<-read.csv(file="dataRepository/MA.limits.csv")

#genus level data for composition score
hc.genus<-read.csv(file="dataRepository/hc.genus.csv")
wq.scores<-read.csv(file="dataRepository/wq.scores.csv")

## ---- Coral Change Indicator ------------------------------------------------------------------------------
#########################################################################
## ---- ApplyGompertz ----                                             ##
## from observed cover data, predicted increase based on annual rate   ##
## of increase for each group estimated from Gompertz equations.       ##
## Estimate scores based on observed  to predicted cover               ##
#########################################################################
#observed data
group.data.reef<-all.reef %>% 
  mutate(estK=100-AB,
         Ohc=round(HC-Acr,5),
         Date=as.Date(Date, "%d/%m/%Y")) %>% 
  arrange(REEF,DEPTH,Date)

## for each visit (for each reef), determine what the next visit
## number will be
group.data.reef = group.data.reef %>% group_by(REEF,DEPTH) %>%
  mutate(nextVISIT_NO = lead(VISIT_NO)) %>% ungroup

####################################################################
## Bind the next years data onto the current row  to faciltate    ##
## predictions based on fitting gompertz model                    ##
####################################################################
coral.reef = group.data.reef %>% inner_join(group.data.reef %>% select(REEF, DEPTH, VISIT_NO,Date,HC,SC,MA,Acr,Ohc,DISTURBANCE),
                                            by=c('REEF'='REEF','DEPTH'='DEPTH','nextVISIT_NO'='VISIT_NO'), suffix=c('','2'))
coral.reef <- coral.reef %>% 
  mutate(duration.days=as.double(difftime(coral.reef$Date2, coral.reef$Date, units="days")),
         Acr2_adj=ifelse(Acr2<Acr, Acr2,Acr+((Acr2-Acr)*(365/duration.days))),
         Ohc2_adj=ifelse(Ohc2<Ohc,Ohc2, Ohc+((Ohc2-Ohc)*(365/duration.days))),
         SC2_adj=ifelse(SC2<SC, SC2, SC+((SC2-SC)*(365/duration.days))),
         HC2_adj=Acr2_adj+Ohc2_adj,
         REEF=factor(REEF),
         P_CODE=as.factor(P_CODE),
         yr=ifelse(P_CODE=="IN", nextVISIT_NO+2004, nextVISIT_NO+1992))
change.reef<-coral.reef  %>% droplevels 

#function to apply gompertz equations to observed data
simulateCoralGrowth=function(change.reef.sub, include.region) {
  if (unique(change.reef.sub$DEPTH)=='2') {
    mod.acr=gomp.acr.2
    mod.ohc=gomp.oth.2
  } else {
    mod.acr=gomp.acr.5
    mod.ohc=gomp.oth.5
  }
  ## Acropora
  RATE=mod.acr[,'rAc']
  wAcMa=rep(0, length(RATE))
  res=mod.acr[,'res']
  Acr=with(change.reef.sub, exp(RATE + log(Acr) + ((-RATE/log(estK))*log(Acr+Ohc+SC+wAcMa*MA)) + res[1]))
  
  ## Other HC
  RATE=mod.ohc$rAc
  wAcMa=rep(0, length(RATE))
  res=mod.ohc$res
  Ohc=with(change.reef.sub, exp(RATE + log(Ohc) + ((-RATE/log(estK))*log(Acr+Ohc+SC+wAcMa*MA)) + res[1]))
  
  HC = cbind(Acr) + cbind(Ohc)
  colnames(HC) <- NULL
  Growth = apply(HC,2, function(x) data.frame(Mean=mean(x, na.rm=TRUE), Median=median(x, na.rm=TRUE), HPDinterval(as.mcmc(x))))
  Growth
}

#rescaling function
rs = function(dat) {
  s = vector(length=nrow(dat))
  for (i in 1:nrow(dat)){
    if (is.na(dat[i,1])) {
      s[i]=NA
    } else {
      s[i]=with(dat, scales::rescale(x[i], from=c(f1[i],f2[i]), to=c(t1[i],t2[i])))
    }
  }
  s
}

#rolling mean  function
rollMean = function(x, width=4) {
  dat <- NULL
  for (i in unique(x$yr)) {
    xx = x %>% filter(yr<(i+1) & yr>(i-width))
    curYear <- max(xx$yr, na.rm=TRUE)
    firstYear <- curYear-3
    DateRange <- paste(firstYear, '-',curYear)
    cs.linear.mean = mean(xx$change.score.linear, na.rm=TRUE)
    dat <- rbind(dat,data.frame(yr=max(xx$yr),DateRange=DateRange,NumYearsInRange=nrow(xx),
                                Change.score.linear.mean=cs.linear.mean                
    ))
  }
  dat
}
#############################################################
## Generate estimated cover increase                     ##
#############################################################

change.reef.all = change.reef %>%
  arrange(VISIT_NO) %>%
  mutate(Dt.num=decimal_date(Date)) %>%
  filter(!is.na(DISTURBANCE2)) %>% 
  tibble::rowid_to_column(.,"N")

change.all = change.reef.all %>%
  group_by(P_CODE,REEF,DEPTH,N) %>% # for each P_CODE,REEF,DEPTH and row 
  do(simulateCoralGrowth(., include.region=FALSE) %>% as.data.frame) %>% # simulate the expected growth (with confidence bands)
  full_join(change.reef.all) %>% # merge back with data
  dplyr::select(everything(),Mean,Median,lower,upper)

save(change.all, file='output/change.all.RData')
#############################################################
## Generate a change scores                                ##
## Note this differs from offshore scores which use different uppper threshold for Ohc and Acr and then combine as a weighted mean of scores for Acr and Ohc##
#############################################################

change.scores= change.all %>%
  filter(DISTURBANCE2 %in% c('n','d')) %>% 
  mutate(change.score.linear = ifelse(HC>HC2_adj, 0.00,
                                      ifelse(HC2_adj<=lower, {dat=data.frame(x=lower-HC2_adj, f1=0, f2=lower-HC, t1=0.4,t2=0.1); rs(dat);}, #scales::rescale(lower-HC2_adj, from=c(0,lower-HC), to=c(0.4,0.1)),
                                             ifelse(lower==0 & upper==0, 0.5,
                                                    ifelse(HC2_adj>lower & HC2_adj<=upper, {dat=data.frame(x=upper-HC2_adj, f1=0,f2=upper-lower, t1=0.6,t2=0.4);rs(dat);},#scales::rescale(upper-HC2_adj, from=c(0,upper-lower), to=c(0.6,0.4)),
                                                           ifelse(HC2_adj>upper & HC2_adj<=(upper+(upper-Median)), {dat=data.frame(x=((upper*2)-Median)-HC2_adj, f1=0, f2=upper-Median, t1=0.9, t2=0.6);rs(dat);},#scales::rescale(((upper*2)-Median)-HC2_adj, from=c(0,upper-Median), to=c(0.9,0.6)),
                                                                  ifelse(HC2_adj>=(2*(upper)-Median), 1, NA)
                                                           )))))) %>% # scale the change scores according to graduated scale
  ungroup

##############################################################
## Add the missing years                                    ##
## 1. create a lookup that includes the reefs and all years ##
## 2. join this with the change scores data                 ##
## 3. put depth back on pre 2005 RM data that is lost above ##
##############################################################
lookup = change.reef %>%
  group_by(P_CODE,REEF,DEPTH) %>%
  complete(yr=seq(2005, max(change.reef$yr, na.rm=TRUE), 1)) %>%
  dplyr::select(P_CODE,REEF,DEPTH,yr) %>%
  distinct

change.scores1 = change.scores %>%
  full_join(lookup) %>% 
  mutate(DEPTH=ifelse(REEF =='Middle Rf LTMP',2,
                      ifelse(P_CODE =='RM' & REEF !='Middle Rf LTMP',5, DEPTH)))

## Add spatial data to infill the NA's produced by above

spatial<-change.scores1 %>% filter(!is.na(NRM_REGION)) %>%
  select(REEF,NRM_REGION,subregion,DEPTH) %>%
  distinct %>%
  mutate(DEPTH=factor(DEPTH))

change.scores2 = change.scores1 %>%
  dplyr::select(-NRM_REGION,-subregion) %>%
  mutate(DEPTH=factor(DEPTH)) %>%
  left_join(spatial)

############################################################
## Aggregate over four year running for each reef/depth.. ##
############################################################
change.scores3 = change.scores2 %>%
  group_by(NRM_REGION,subregion,P_CODE,REEF,DEPTH) %>%
  arrange(-yr) %>%
  do(rollMean(., width=4)) %>%
  ungroup %>%
  arrange(REEF, DEPTH, yr) %>%
  mutate(DEPTH=factor(DEPTH))

coral = all.reef %>%
  left_join(MA.limits) %>%
  mutate(yr=ifelse(P_CODE=="IN", VISIT_NO+2004,VISIT_NO+1992),
         DEPTH=factor(DEPTH)) 

coral = coral %>% full_join(change.scores3)

## ---- Macroalgae Cover Indicator --------------------------------------------------------------------------

coral = coral %>% 
  mutate(MAprop=(MA/A)*100) %>%
  tibble::rowid_to_column(.,'N') %>%
  group_by(N) %>%
  mutate(ma.score = ifelse(MAprop< MAlower, 1,
                           ifelse(MAprop>= MAlower & MAprop <= MAupper,{dat=data.frame(x=MAprop, f1=MAlower,f2=MAupper, t1=1,t2=0);rs(dat);}, 0)
  )) %>%
  ungroup

## ---- Coral Cover Indicator -------------------------------------------------------------------------------

lBound = 0
uBound = 75

coral = coral %>%
  mutate(CoralCover.score = ifelse(CoralCover<lBound,0,
                                   ifelse(CoralCover>=lBound & CoralCover<=uBound, rescale(CoralCover, from=c(lBound,uBound), to=c(0,1)), 1)
  ))

## ---- Juvenile Density Indicator---------------------------------------------------------------------------

lBound <- 0
mid<-4.6
uBound <- 13

coral = coral %>%
  mutate(juv.score = ifelse(juv5.d.nf<mid, rescale(juv5.d.nf, from=c(lBound,mid), to=c(0,0.4)),
                            ifelse(juv5.d.nf>=mid & juv5.d.nf <uBound, rescale(juv5.d.nf, from=c(mid,uBound), to=c(0.4,1)),1)
  ))

## ---- Coral composition Indicator -------------------------------------------------------------------------

########################################################################
## merge weights onto observations and calculate wq relevent score for ##
## hellinger transformed genus covers for each observation             ##
#########################################################################

hc.hel<-hc.genus %>% select(P_CODE,REEF,DEPTH,VISIT_NO,NRM_REGION,subregion, ACANTHASTREA:pb)
hc.hel[,7:ncol(hc.hel)]<-vegan::decostand(hc.hel[,7:ncol(hc.hel)], method="hellinger")

hc.long<-hc.hel %>% group_by(P_CODE,NRM_REGION,subregion,REEF,DEPTH,VISIT_NO) %>%
  gather(key=genus, value=cover, ACANTHASTREA:pb)

hc.com.scores<-left_join(hc.long,wq.scores) %>% ungroup %>%
  mutate(DEPTH=as.factor(DEPTH))
hc.com.scores1<-left_join(hc.com.scores, spatial)
hc.composition.wq <-hc.com.scores1 %>% mutate(gen.composition=cover*weight) %>%
  dplyr::select(-cover,-weight) %>%
  group_by(P_CODE,NRM_REGION,subregion,REEF,VISIT_NO,DEPTH) %>%
  dplyr::summarise(gen.composition=sum(gen.composition, na.rm=TRUE)) %>%
  mutate(yr=ifelse(P_CODE=="IN", VISIT_NO+2004,VISIT_NO+1992))

new.data = hc.composition.wq %>% ungroup %>% filter(!is.na(subregion)) %>%
  group_by(REEF,DEPTH) %>%
  nest() %>%
  mutate(Model = map(.x=data, ~lm(gen.composition~1, data=.[1:5,]))) %>%
  mutate(Conf.int = map(.x=Model, .f=function(mod) data.frame(lower=gmodels::ci(mod)[2], upper=gmodels::ci(mod)[3]))) %>%
  unnest(Conf.int) %>%
  unnest(data)

community.control.chart.limits = new.data %>%
  mutate(improve=gen.composition-upper,
         decline=lower-gen.composition,
         score=ifelse(improve>0,"green",ifelse(decline>0, "red","black")),
         composition.score=ifelse(improve>0,1,ifelse(decline>0,0,0.5)))

lookup = community.control.chart.limits %>%
  group_by(NRM_REGION,subregion,P_CODE,REEF,DEPTH) %>%
  complete(yr=seq(2005, max(change.reef$yr, na.rm=TRUE), 1)) %>%
  dplyr::select(NRM_REGION,subregion,P_CODE,REEF,DEPTH,yr) %>%
  distinct

composition.indicator.scores = community.control.chart.limits %>%
  full_join(lookup) %>%
  group_by(NRM_REGION,subregion,REEF,P_CODE,DEPTH) %>%
  arrange(REEF, P_CODE,DEPTH,yr) %>%
  fill(composition.score, .direction='down') %>%
  ungroup

## Merge the composition scores into the other coral indicators
coral = coral %>% dplyr::select(-MAupper,-MAlower) %>%
  left_join(composition.indicator.scores %>% dplyr::select(-P_CODE,-NRM_REGION,-upper,-lower,-improve,-decline,-score,-VISIT_NO) %>%
              mutate(DEPTH=factor(DEPTH)))

## ---- final tidying up ----------------------------------------------------------------------------------
coral= coral %>%
  arrange(REEF,P_CODE,DEPTH,yr) %>%
  group_by(NRM_REGION,subregion,REEF,P_CODE,DEPTH) %>%
  fill(CoralCover.score, .direction='down') %>%
  fill(ma.score, .direction='down') %>%
  fill(juv.score, .direction='down') %>%
  ungroup


########################
# Fix for King Reef discontinuation
########################

coral<- coral %>% filter(!(REEF=='King' & yr>2014))

############################
# Remove duplication of Middle reef from scoring
#############################
coral = coral %>% filter(!(REEF=="Middle Rf" & yr>2014),
                         !(REEF=="Middle Rf LTMP" & yr<2015))

############################
# Remove NA pre sampling for Bedarra and compositioin score as no baseline yet set
#############################
coral=coral %>% filter(!(REEF=="Bedarra" & yr<2015)) %>%
  mutate(composition.score=ifelse(REEF=="Bedarra", NA, composition.score))

save(coral, file='output/coral.RData')


