#setwd('C:/Users/uk/Projects/Navigate/AlternativeWelfareMetrics/github')
setwd('C:/Users/uk/Documents/GitHub/multidimwelfare-scenarios')
rm(list = ls())

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

#add per-capita values to dataframe: variable name "variables_pop"+"|PerCapita"
#ar6_datadf=add_per_capita_df(ar6_datadf, variables_pop)

# specify minimum and maximum for all variables 
# (for per-capita variables these should already be in per-capita terms)
# if no maximum/minimum can be a priori specified, specify "NA" and the maximum/minimum across all models scenarios will be taken

# for consumption/gdp this is 0.1 thousand USD per capita
# for emissions this is zero (yes for co2)?
# what should it be for land cover?

#ensure order of the following list is the same as in (variables_min/max)
# minimum for both food items is 1827 kcal/cap/day (global average from FAO: https://bit.ly/FoodSecIndicators20 )
#   value for food energy supply is transformed to EJ/year/cap ( =*4184*365/10^(12) )
variables_min=c(0, 0.1, NA, NA, NA, NA, 0.1, NA, NA, NA, 0.00279, 1827);
names(variables_min)=variables
variables_max=c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA);	
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
# these are a dataframe for different sets of weights: rows are different sets, columns are variables 


weight1=c(1,0,0,0.5,0.5,1,100,0,1,0,1,0)
names(weight1)=variables
weight2=c(1,0,0,0.5,0.5,1,1  ,0,1,0,1,0)
names(weight2)=variables
weight3=c(1,0,0,0.5,0.5,1,0.2,0,1,0,1,0)
names(weight3)=variables
weights=list(a=weight1, b=weight2, c=weight3)

####################################################################################################    
###############################     compute welfare     ############################################
####################################################################################################    


welfares=compute_welfare_df(indicators, rho, weights)


ar6_datadf$identifier<-NULL





	
	

