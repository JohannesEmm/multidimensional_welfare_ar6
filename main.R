rm(list = ls())


library(tidyverse)
library(ggpubr)
library(imputeTS)
library(data.table)
library(RColorBrewer)
library(latex2exp)
library(gridExtra)
library(RColorBrewer)
library(PerformanceAnalytics)
library(GGally)

### if welfares already exist, just load Rdata file
### if welfares exist, indicator.Rdata and weights.Rdata is assumed to exist as well, so this will also load


if(!file.exists("welfares.Rdata"))
  {
  ####################################################################################################    
  #########################        load functions       ##############################################
  ####################################################################################################    
  
  source("functions.R")
  
  
  
  ####################################################################################################    
  #########################        load dataframe       ##############################################
  ####################################################################################################    
  if(!file.exists("ar6_data.Rdata")) source("get_scenario_data.R")
  df=load("ar6_data.Rdata");
  
  ####################################################################################################    
  #########################   specify column names that uniquely determine scenario   ################
  ####################################################################################################    
  
  # add column that identifies the scenario 
  ar6_datadf$identifier <- paste(ar6_datadf$model, ar6_datadf$scenario, ar6_datadf$year, sep="XXX")
  
  ####################################################################################################    
  #####################     variables for welfare metric      ########################################
  ####################################################################################################    
  
  # specify variables to be included in welfare metric (these have to be in the dataframe):
  variables=unique(ar6_datadf$variable)
  print("#### List of variables to be used for min/max and weight definition below: #####")
  print(variables)
  if (!is_empty(setdiff("Population",variables))){abort("Error: scenario data does not contain population")}
  
  #specify which variables are evaluated with the log for the indicator value (these are usually consumption and GDP)
  variables_log=c("Consumption","GDP|PPP"); 
  if (!is_empty(setdiff(variables_log,variables))){abort("Error: list of log-variables not contained in variables")}
  
  #specify which variables are bads (i.e. welfare is decreasing in them)
  variables_bad=c("Emissions|Sulfur","Emissions|NOx","Emissions|CO2","AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile");
  if (!is_empty(setdiff(variables_bad,variables))){abort("Error: list of bad-variables not contained in variables")}
  
  #specify variables that have to be on a per-capita basis from list above (internal computation):
  variables_pop=c("Emissions|Sulfur","Emissions|NOx","Consumption","Emissions|CO2","GDP|PPP", "Final Energy|Electricity","Food Energy Supply");
  if (!is_empty(setdiff(variables_pop,variables))){print("Error: list of per-capita-variables not contained in variables")}
  
  # specify minimum and maximum for all variables 
  # for per-capita variables these should already be in per-capita terms
  # for forest land cover, switching to share of forest in land
  # if no maximum/minimum can be a priori specified, specify "NA" and the maximum/minimum across all models scenarios will be taken
  
  #ensure order of the following list is the same as in (variables_min/max)
  # source of extrema is documented in excel sheet AR6_min_max
  # extrema from the data set are calculated in CheckOutliers.R file
  variables_min=c(0, 0.402, -26 , 0.000493, 0.000115, 0,0.1, NA, 0   , NA, 0.00279, 1827);
  names(variables_min)=variables
  variables_max=c(NA, 161 , 35.1, 0.059   , 0.0575  , 0.254 , 75, NA, 0.61, NA, 0.00688, 4505);	
  names(variables_max)=variables
  
  #For each variable, the indicator will be added along with minimum, maximum, indicator for bad and log
  indicators=add_indicators_df(ar6_datadf, variables_log, variables_bad, variables_pop, variables_min, variables_max)
  
  
  ####################################################################################################    
  ##########################     normative parameters     ############################################
  ####################################################################################################    
  
  #substitutability between dimensions of welfare metric
  rho=c(0,1,5);
  
  # welfare weights: specify relative weights of each dimension
  # list of variables that receive positive weight:
  vars_with_pos_weights=c("AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile", "Emissions|NOx" , "Emissions|Sulfur", "Final Energy|Electricity" , "GDP|PPP", "Land Cover|Forest" ,"Food Energy Supply");
  # specify relative weights:
  rel_weights=c(100,1,0.01);
  # specify whether nox and sox should be put into 1 category, thus receiving each half the weight
  yes_snox=1;
  
  weights1=partition_variables(variables,vars_with_pos_weights,rel_weights,yes_snox)
  
  # put weights in original order of variables
  weights=weights1[,variables]
  
   
  ####################################################################################################    
  ###############################     compute welfare     ############################################
  ####################################################################################################    
  
  
  welfares=compute_welfare_df(indicators, rho, weights)

  ####################################################################################################    
  ###############################     some cosmetics before saving dataframes   ######################
  ####################################################################################################    
  
  # reduce welfares-dataframe to those of SSP2 and where vetting did not fail, drop C8 scenario  
  welfares <- welfares %>%
    filter(Category!="C8")%>%
    filter(Ssp_family==2)%>%
    dplyr::filter(Category!="failed-vetting")
  
  # add column that identifies the scenario with time
  welfares$identifier <- paste(welfares$model, welfares$scenario, welfares$year, sep="XXX")
  
  # add column that identifies weights
  welfares = welfares %>% unite(col="weights",c(14:25), sep = ", ", remove = FALSE, na.rm = FALSE)
  
  # add column that identifies the three selected sets of weights with low, med and high GDP
  welfares = welfares %>%
    mutate(selected_weights= if_else(weights %in% c("0.01, 0, 0, 0.005, 0.005, 0.01, 1, 0, 0.01, 0, 0.01, 0", "0.01, 0, 0, 0.005, 0.005, 0.01, 0.01, 0, 0.01, 0, 0.01, 0", "1, 0, 0, 0.5, 0.5, 1, 0.01, 0, 1, 0, 1, 0"), weights, NA))
  
  # save output
  save(welfares, file = "welfares.Rdata") 
 
  
  ###############################rename variables#####################################################
  
  indicators$variable[indicators$variable == "AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile"]="Temperature"
  indicators$variable[indicators$variable == "Final Energy|Electricity"]="Electricity"
  indicators$variable[indicators$variable == "Emissions|NOx"]="NOx Emissions"
  indicators$variable[indicators$variable == "Emissions|Sulfur"]="Sulfur Emissions"
  indicators$variable[indicators$variable == "GDP|PPP"]="GDP"
  indicators$variable[indicators$variable == "Population"]="Population"
  indicators$variable[indicators$variable == "Food Energy Supply"]="Food Supply"
  indicators$variable[indicators$variable == "Land Cover|Forest"]="Forest Cover"
  
  
  
  save(indicators, file = "indicators.Rdata") 
  
  
  
  
  ar6_datadf$identifier<-NULL
  
}else{
  load("indicators.Rdata")
  load("welfares.Rdata")
  load("ar6_data.Rdata")
}





	







#########  PLOTS ##############
source("plots.R")




