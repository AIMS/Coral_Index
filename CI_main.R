## The following script constitutes the main code sequencing thread for codes to
## accompany the manuscript 'Development Of The Coral Index, A Summary Of Coral
## Reef Resilience As A Guide For Management'

## source required .csv data files from metadata record
## doi *https://apps.aims.gov.au/metadata/view/7c6101f9-50a6-46fb-afe9-c16bd09334d0)
## 1. download the zip from the above source
## 2. open the zip and navigate in to the dataRepository folder
## 3. extract this folder (and all its contents) to the root of the current repository

## The scripts have the following dependencies.
## coda, scales, tidyverse, lubridate, gmodels, gamlss, MuMIn, gridExtra, brms, broom, effects, gtable,grid,cowplot,mgcv,ggplot2

library(coda)
library(scales)
library(tidyverse)
library(lubridate)
library(gmodels)
library(gamlss)
library(MuMIn)
library(gridExtra)
library(brms)
library(broom)
library(effects)
library(gtable)
library(grid)
library(cowplot)
library(mgcv)

## Preparations
if(!dir.exists('output')) dir.create('output')

source('CI_estimate_scores.R')
#################################################################
# read in observed data from which indicator values are estimated 
#   all.reef.csv
# These data are extracted from AIMS reefmonitoring database
#       fields include:
#                 P_CODE- identifies either the AIMS Long-term Monitoring program 'RM' or Marine Monitoring Program 'IN' as the source of data
#                     that facilitates some necessary data processing due to differeing sampling designs among the projects.
#                 NRM_REGION - is a spatial reporting unit used to aggregate scores.
#                 REEF - sampling locations within NRM_REGIONS.
#                 DEPTH - depth of transects below lowest astronomic tide.
#                 VISIT_NO - a sequential, P_CODE specific, tag used to link samples from a REEF, DEPTH and time for reporting.
#                 Date - the date of observation.
#                 subregion -  subsets the Wet Tropics NRM_REGION for reporting
#                 A - proportional cover of the benthos occupied by visible Algae, includes all Crustose coralline, turf, aad larger fleshy species.
#                 AB - proportional cover of the benthos occupied by sediments (sand and mud)
#                 HC - proportional cover of the benthos occupied by Hard corals (Scleractinia)
#                 SC - proportional cover of the benthos occupied by Soft corals (Octocorallia) 
#                 CoralCover - the sum of HC and SC
#                 MA - proportional cover of the benthos occupied by macroalgae, excludes crustose coralline and turfing forms
#                 Acr - proportional cover of the benthos occupied by hard corals of the family Acroporidae
#                 DISTURBANCE - categorical field identifying major disturbances, n=none, f=flood, d=disease, s=cyclone/storm, c=crown-of-thorns, m=multiple, d=disease
#                 juv5.d.nf - density of juvenile corals, exclusive of the genus Fungia.
#
# distibutions of gompertz equation parameter estimates used to predict coral cover increase
#  gomp.acr.5.csv - esimates for family acroproidae at 5m depth
#  gomp.acr.2.csv - esimates for family acroproidae at 2m depth
#  gomp.oth.5.csv - esimates for all other hard corals at 5m depth
#  gomp.oth.2.csv - esimates for fall other hard corals at 2m depth
#
# estimated limits for scoring Macroalgae indicator
#  MA.limits.csv
#       fields include: 
#                 REEF,DEPTH as above.
#                 MAupper - upper threshold for MA scoring based on predicted proportion of MA in algae community given the mean water quality of the reef (Thompson et al. 2014)
#                 MAlower - lower threshold for MA scoring based on predicted proportion of MA in algae community given the mean water quality of the reef (Thompson et al. 2014)
# observed cover of hard corals identified to genus 
#   hc.genus.csv
#       fields include: 
#                 P_CODE,NRM_REGION,subregion,REEF,DEPTH,VISIT_NO as above.
#                 Columns for for cover of each genus.
# genus scores on rda WQ axis. These were created from the analysis included as a case study in Thompson et al 2014
#   wq.scores.csv:
#       fields include: 
#                 weight - the species score on the constrained (water quality gradient) axis of a redundancy analysis 
#                 genus - hard coral genus
#                 DEPTH -  the depth to which the weight applies.
#
# OUTPUT 
#   output/change.all.RData - observed and predicted coral cover for all observations
#   output/coral.RData
##############################################################################################################

source('coralBoot.R')
##########################################################################################################
# bootstrap scores for individual indicators to provide distribution of index scores from which
# confidence intervals in index scores are derived.
# Input
#   output/coral.RData
# Output
#   output/boot.region.metric.sum.RData
#   output/boot.region.score.sum.RData
##################################################################################################

source('Figure1.R')
#########################################################################################
# plotting and supporting analysis for Figure 1
#Input
#   output/coral.RData
#   bom.month.csv - data sourced from the Bureau of Meterology
#       fields include: 
#                 REEF - as above
#                 data - mean of estimates from a cluster of nine pixels adjacent to each reef
#                 variableName -  Nap_MIM_median monthly median concentration of Non-algal particulates
#                                 CDOM_MIN_median... colour dissolved organic material concentration
#                                 Kd_490....... light attenuation coefficient
#                                 Chl_......... Chl a concentration
#                 Month - the month of the Year data were observed
#                 Year - the year data were observed
#   jad.reef.csv - juvenile density data sourced from AIMs LTMP and MMP database
#       fields include:
#                 REEF - as above
#                 VISIT_NO - as above
#                 DEPTH - as above
#                 Acroporidae - mean number of juvenile (< 5cm) Acroporidae per site
#                 Dendrophyllidae - mean number of juvenile (<5cm) Dentrophylliidae - Predominantly Turbinaria, per site
#                 propAC - relative proportion of juveniles that were Acroporidae
#                 
# Output = Figure 1.
#        = Figure S1.
###########################################################################################

source('Figure2.R')
#########################################################################################
# plotting of Figure 2
#Input
#   output/coral.RData
#   output/boot.region.metric.sum.RData
#   output/boot.region.score.sum.RData
#       
# Output = Figure 2.
###########################################################################################

source('Figure3.R')
#########################################################################################
# plotting and supporting analysis for Figure 3 and Table 4
#Input
#   output/coral.RData
#   
# Output = Figure 3.
#        = Table 4
###########################################################################################

source('Figure4.R')
#########################################################################################
# plotting and supporting analysis for Figure 4 and Table 5
#Input
#   output/coral.RData
#   data/discharge.annual.RData - data sourced from the the Queensland government water monitoring information portal
#                                 https://water-monitoring.information.qld.gov.au/host.htm
#       fields include: 
#                 subregion - as above
#                 Year - the water Year (October 1 to September 31) as at September 31.
#                 discharge.c.annual - combined annual discharge from gauged major rivers discharging into the subregion that include:
###---- Daintree
# Daintree River - 108002A
# Mossman River - 109001A
# Barron River 110001D
#
###----Johnstone
# Mulgrave River - 111007A
# Russell River - 111101D
# North Johnstone - 112004A
# South Johnstone - 112101B
#
###------Tully
# Tully River - 113006A
# Murray River - 114001A
# Herbert River - 116001F, 116001E (change over 2009)
#
###------Burdekin
# Black River - 117002A
# Haughton River - 119003A
# Burdekin River - 120006B
# Don River - 121003A
#
###------Proserpine
# Proserpine River - 122004A
# O'Connell River - 124001B 
# Pioneer River - 1250007A
# Sandy creek - 126001A
# Carmila Creek - 126003A
#
###-----Fitzroy
# Waterpark Creek - 129001A 
# Fitzroy River - 130005A
#
# Output = Figure 4.
###########################################################################################

source('temperature figure S2.R')
#########################################################################################
# data manipulation and plotting of Figure S2
#Input
#   dataRepository/waterTemp
#   dataRepository/temperature.lookup - data sourced from the Beureau of Meterology
#                                 https://water-monitoring.information.qld.gov.au/host.htm
#       fields include: 
#                 subregion - as above
#                 Year - the water Year (October 1 to September 31) as at September 31.
#                 discharge.c.annual - combined annual discharge from gauged major rivers discharging into the subregion
#
# Output = FigureS2.
###########################################################################################
