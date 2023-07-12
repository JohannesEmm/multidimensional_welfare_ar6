#setwd('C:/Users/uk/Projects/Navigate/AlternativeWelfareMetrics')
#setwd('C:/Users/uk/Documents/GitHub/multidimwelfare-scenarios')

rm(list = ls())

### if welfares already exist, just load Rdata file
### if welfares exist, indicator.Rdata and weights.Rdata is assumed to exist as well, so this will also load

if(!file.exists("welfares.Rdata"))
  {
  

  
  library(ggplot2)
  library(dplyr)	
  library(stringr)
  library(tidyverse)
  library(imputeTS)
  library(data.table)
  
  ####################################################################################################    
  #########################        load functions       ##############################################
  ####################################################################################################    
  
  source("compute_welfare_df.R");
  source("compute_welfare.R");
  source("add_indicators_df.R");
  source("partition_variables.R");
  
  
  ####################################################################################################    
  #########################        load dataframe       ##############################################
  ####################################################################################################    
  
  file_name='ar6_data.Rdata';
  df=load(file_name);
  
  
  
  ####################################################################################################    
  #########################   specify column names that uniquely determine scenario   ################
  ####################################################################################################    
  
  # add column that identifies the scenario with (NOT IMPLEMENTED region and) time
  ar6_datadf$identifier <- paste(ar6_datadf$model, ar6_datadf$scenario, ar6_datadf$year, sep="XXX")
  
  ####################################################################################################    
  #####################     variables for welfare metric      ########################################
  ####################################################################################################    
  
  # specify variables to be included in welfare metric (these have to be in the dataframe):
  variables=unique(ar6_datadf$variable)
  print("#### List of variables to be used for min/max and weight definition below: #####")
  print(variables)
  if (!is_empty(setdiff("Population",variables))){abort("Error: scenario data does not contain population")}
  
  #specify which are evaluated with the log (these are usually consumption and GDP)
  variables_log=c("Consumption","GDP|PPP"); 
  if (!is_empty(setdiff(variables_log,variables))){abort("Error: list of log-variables not contained in variables")}
  
  #specify which variables are bad (i.e. welfare is decreasing in them)
  variables_bad=c("Emissions|Sulfur","Emissions|NOx","Emissions|CO2","AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile");
  if (!is_empty(setdiff(variables_bad,variables))){abort("Error: list of bad-variables not contained in variables")}
  
  #specify variables out of list with regional inequality assessment
  #NOT IMPLEMENTED variables_regional=c();
  
  #specify variables that have to be on a per-capita basis from list above (internal computation):
  variables_pop=c("Emissions|Sulfur","Emissions|NOx","Consumption","Emissions|CO2","GDP|PPP", "Final Energy|Electricity","Food Energy Supply");
  if (!is_empty(setdiff(variables_pop,variables))){print("Error: list of per-capita-variables not contained in variables")}
  
  # specify minimum and maximum for all variables 
  # for per-capita variables these should already be in per-capita terms
  # for forest land cover, switching to share in land
  # if no maximum/minimum can be a priori specified, specify "NA" and the maximum/minimum across all models scenarios will be taken
  
  #ensure order of the following list is the same as in (variables_min/max)
  # finding extrema is documented in excel sheet AR6_min_max_updates
  # extrema from the data set are calculated in CheckOutliers.R file
  variables_min=c(0, 0.402, -26 , 0.000493, 0.000115, 0.0009,1.62, NA, 0        , NA, 0.00279, 1827);
  names(variables_min)=variables
  variables_max=c(NA, 161 , 35.1, 0.059   , 0.0575  , 0.254 , 180, NA, 0.61, NA, 0.00688, 4505);	
  names(variables_max)=variables
  
  #For each variable, the indicator will be added along with minimum, maximum, indicator for bad and log
  indicators=add_indicators_df(ar6_datadf, variables_log, variables_bad, variables_pop, variables_min, variables_max)
  
  
  ####################################################################################################    
  ##########################     normative parameters     ############################################
  ####################################################################################################    
  
  # inequality aversion:
  #NOT IMPLEMENTED epsilon=c(0,1,1.5);
  
  #substitutability between dimensions of welfare metric
  rho=c(0,1,5);
  
  #welfare weights: specify relative weights of each dimension
  #list of variables that receive positive weight:
  vars_with_pos_weights=c("AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile", "Emissions|NOx" , "Emissions|Sulfur", "Final Energy|Electricity" , "GDP|PPP", "Land Cover|Forest" ,"Food Energy Supply");
  #specify relative weights:
  rel_weights=c(100,1,0.01);
  #specify wether nox and sox should be put into 1 category, thus receiving each half the weight
  yes_snox=1;
  
  weights=partition_variables(variables,vars_with_pos_weights,rel_weights,yes_snox)
  
  
  ####################################################################################################    
  ###############################     compute welfare     ############################################
  ####################################################################################################    
  
  
  welfares=compute_welfare_df(indicators, rho, weights)

  ####################################################################################################    
  ###############################     some cosmetics before saving dataframes#############################
  ####################################################################################################    
  
    
  welfares <- welfares %>%
    filter(Category!="C8")%>%
    dplyr::filter(Category!="failed-vetting")
  
  # add column that identifies the scenario with (NOT IMPLEMENTED region and) time
  welfares$identifier <- paste(welfares$model, welfares$scenario, sep="XXX")
  
  # add column to identify the low, equal, and high gdp weight scenario:
  weight1=c(1,0,0,0.5,0.5,1,100,0,1,0,1,0)
  names(weight1)=variables
  weight2=c(1,0,0,0.5,0.5,1,1  ,0,1,0,1,0)
  names(weight2)=variables
  weight3=c(1,0,0,0.5,0.5,1,0.01,0,1,0,1,0)
  names(weight3)=variables
  #save these selected weights 
  weights=list(a=weight1, b=weight2, c=weight3)
  save(weights, file = "weights.Rdata") 
  
  
  welfares[,"weights"]=NA;
  
  welfares$weights[rowSums(welfares[,c(14,16,17,18,19)]==0.01)==5 & rowSums(welfares[,c(15,20)]==0.005)==2]<-paste(weight2, sep=" ", collapse=",")#equal weights scenario gets identified by old name
  welfares$weights[rowSums(welfares[,c(14,16,18,19)]==0.01)==4 & rowSums(welfares[,c(15,20)]==0.005)==2 & welfares[,17]==1]<-paste(weight1, sep=" ", collapse=",")#high GDP scenario gets identified by old name
  welfares$weights[rowSums(welfares[,c(14,16,18,19)]==1   )==4 & rowSums(welfares[,c(15,20)]==0.5  )==2 & welfares[,17]==0.01]<-paste(weight3, sep=" ", collapse=",")#low GDP scenario gets identified by old name
  
  #save output
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
  load("weights.Rdata")
}





	
	

