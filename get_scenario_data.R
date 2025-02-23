require(tidyverse)  
require(reticulate)

reload_data <- T

#list of variables
varlist <- c("Emissions|NOx", "Emissions|Sulfur", "Emissions|CO2", "Consumption", "GDP|PPP", "Population", "Land Cover", "Land Cover|Forest", "AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile", "Food Energy Supply", "Food Demand", "Final Energy|Electricity")


#for using python, make sure the path to it is specied using 
#file.edit(file.path("~", ".Rprofile"))
Sys.which('python')



if(!file.exists("ar6_data.Rdata") | reload_data){
  #AR6 data
  pyam <- import("pyam", convert = FALSE)
  
  #explore the source
  conn = pyam$iiasa$Connection('ar6-public')
  conn$models()$head()
  conn$regions()$head()
  variables <- data.frame(variable=py_to_r(conn$variables()))
  
  #load variables
  ar6_data <- pyam$read_iiasa('ar6-public', model='*', scenario="*", variable=varlist, region='World', meta=1)
  #as_pandas concatenates data and meta into a pandas DF (meta_cols = TRUE adds all meta data)
  ar6_datadf <- ar6_data$as_pandas(meta_cols = c("Ssp_family", "Policy_category", "Policy_category_name", "Category", "IMP_marker"))
  #pandas to R data frame
  ar6_datadf <- py_to_r(ar6_datadf)
  #all categories are lists, convert to simple vectors
  Policy_category <- data.frame(Policy_category=unlist(ar6_datadf$Policy_category))
  Policy_category_name <- data.frame(Policy_category_name=unlist(ar6_datadf$Policy_category_name))
  Category <- data.frame(Category=unlist(ar6_datadf$Category))
  ar6_datadf <- ar6_datadf %>% select(-c("Policy_category", "Policy_category_name", "Category"))
  ar6_datadf <- cbind(ar6_datadf, Policy_category, Policy_category_name, Category)
  #write final data frame as Rdata file
  save(ar6_datadf, variables, file = "ar6_data.Rdata")
  
  #now get also regional data
  ar6_data_regional <- pyam$read_iiasa('ar6-public', model='*', scenario="*", variable=varlist, region='* (R6)', meta=1)
  ar6_data_regional <- ar6_data_regional$as_pandas(meta_cols = c("Ssp_family", "Policy_category", "Policy_category_name", "Category", "IMP_marker"))
  ar6_data_regional <- py_to_r(ar6_data_regional)
  save(ar6_data_regional, file = "ar6_data_regional.Rdata")  
}else{
  load("ar6_data.Rdata")
  load("ar6_data_regional.Rdata")
}

#some diagnostics plots
print(ggplot(ar6_datadf %>% dplyr::filter(Category!="failed-vetting" & Category!="NaN" & Category!="no-climate-assessment")) + geom_line(aes(year, value, group=interaction(scenario, model), color=Category), alpha=0.7) + facet_wrap(variable ~ ., scales = "free") + theme(legend.position = "bottom"))


##############extracting extrema###############################


#get regional data (R6) regions
#get per capital values
ar6_data_regional <- ar6_data_regional %>% left_join(ar6_data_regional %>% filter(variable=="Population") %>% select(-variable,-unit) %>% rename(Population=value))
#remove outliers (see script CheckOutliers.R)
ar6_data_regional <- ar6_data_regional %>% filter(Population < 1*10^5 & !(variable=="Food Demand" & value > 10000) & !(variable=="Food Energy Supply" & value == 0) & !(variable=="Land Cover" & value < 10000) & !(variable=="Land Cover|Forest" & value < 2000))
#compute per capita values
ar6_data_regional <- ar6_data_regional %>% mutate(valuepc=value/Population)
#get extrema
ar6_data_regional_extremes <- ar6_data_regional %>% group_by(variable) %>% summarize(min=min(valuepc, na.rm = T), max=max(valuepc, na.rm = T))
print(ar6_data_regional_extremes)

#add forest land cover as share:

ar6_data_regional <- ar6_data_regional %>% left_join(ar6_data_regional %>% filter(variable=="Land Cover") %>% select(-variable,-unit) %>% rename(Land=value))
ar6_data_regional <- ar6_data_regional %>% mutate(valuepc_land=value/Land)
#get extrema
ar6_data_regional_extremes <- ar6_data_regional %>% group_by(variable) %>% summarize(min=min(valuepc_land, na.rm = T), max=max(valuepc_land, na.rm = T))
print(ar6_data_regional_extremes)

#add temperature:
ar6_data_extremes <- ar6_datadf %>% group_by(variable) %>% summarize(min=min(value, na.rm = T), max=max(value, na.rm = T))
print(ar6_data_extremes)


                   

     