#setwd('C:/Users/uk/Projects/Navigate/AlternativeWelfareMetrics')

rm(list = ls())

	library(readxl)
	library(officer)
	library(flextable)
	library(htmlTable)
	library(magrittr)
	library(kableExtra)
#	library(magick)
	library(ggplot2)
	library(ggforce)
  
  library(tidyverse)

####################################################################################################    
#########################        load functions       ##############################################
####################################################################################################    

	source("compute_welfare_df.R");
source("compute_welfare.R");
	source("add_per_capita_df.R");
	source("add_min_max.R");



####################################################################################################    
#########################        load dataframe       ##############################################
####################################################################################################    

	#file_name='SSP_IAM_V2_201811.csv'
	#df=read.csv(file=file_name)
	file_name='ar6_data.Rdata';
	df=load(file_name);

####################################################################################################    
#########################   specify column names that uniquely determine scenario   ################
####################################################################################################    

# add column that identifies the scenario with region and time
      ar6_datadf$identifier <- paste(ar6_datadf$model, ar6_datadf$scenario, # NOT IMPLEMENTED ar6_datadf$region,
						 ar6_datadf$year, ar6_datadf$Ssp_family, 
						 ar6_datadf$Policy_category, ar6_datadf$Policy_category_name, 
						 ar6_datadf$Category_FaIRv1.6.2, ar6_datadf$Category ,
						 ar6_datadf$Vetting_future, ar6_datadf$Vetting_historical,
						 ar6_datadf$IMP_marker, sep="_")

# add column that identifies the scenario without time
      ar6_datadf$identifierWOregion <- paste(ar6_datadf$model, ar6_datadf$scenario, # NOT IMPLEMENTED ar6_datadf$region,
						 	ar6_datadf$Ssp_family, 
						 	ar6_datadf$Policy_category, ar6_datadf$Policy_category_name, 
						 	ar6_datadf$Category_FaIRv1.6.2, ar6_datadf$Category ,
						 	ar6_datadf$Vetting_future, ar6_datadf$Vetting_historical,
						 	ar6_datadf$IMP_marker, sep="_")



####################################################################################################    
#####################     variables for welfare metric      ########################################
####################################################################################################    

# specify variables to be included in welfare metric (these have to be in the dataframe):

	variables=c("Consumption", "Land Cover|Forest", "AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile");

	#specify which variables are bad (i.e. welfare is decreasing in them)
	variables_bad=c("AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile");

	#specify variables out of list with regional inequality assessment
	#NOT IMPLEMENTED variables_regional=c();

# specify variables that have to be on a per-capita basis from list above (internal computation):

	variables_pop=c("Consumption");

	#add per-capita values to dataframe: variable name "variables_pop"+"|PerCapita"
	ar6_datadf=add_per_capita_df(ar6_datadf, variables_pop)

# specify minimum and maximum for all variables 
# (for per-capita variables these should already be in per-capita terms)
# if no maximum/minimum can be a priori specified, specify "NA" and the maximum/minimum across all scenarios will be taken

# for consumption this is 0.1 thousand USD per capita
# for emissions this is zero (yes for co2)?
# what should it be for land cover?

	variables_min=c(0.1, "NA", 0);
	variables_max=c("NA", "NA", "NA");	
 
	#add minima and maxima across all scenarios if non given before:
	res=add_min_max(ar6_datadf, variables, variables_pop, variables_min, variables_max)
	variables_min=res$mins
	variables_max=res$maxs

	

####################################################################################################    
##########################     normative parameters     ############################################
####################################################################################################    

	# inequality aversion:
	#NOT IMPLEMENTED epsilon=c(0,1,1.5);
	
	#substitutability between dimensions of welfare metric
	rho=c(0,1,2);

	#welfare weights: specify the ratio of weighting consumption against all other variables
	#1: all variables (e.g. consumption, emissions, biodiversity) get the same weight
	#2: consumption gets double the weight compared to all other variables, all other variables have the same relative weight
	weight=10;

####################################################################################################    
###############################     compute welfare     ############################################
####################################################################################################    

	
	welfares=compute_welfare_df(ar6_datadf, variables, variables_pop, variables_min, variables_max, variables_bad, rho, weight)



	
	
	
	
	ggplot(welfares %>% dplyr::filter(Vetting_future=="Pass" & Vetting_historical=="Pass") %>% dplyr::filter(!is.nan(value))) + geom_line(aes(year, value, group=interaction(scenario, model), color=Category_FaIRv1.6.2), alpha=0.7)

