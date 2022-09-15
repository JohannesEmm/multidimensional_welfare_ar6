require(data.table)
require(tidyverse)

reload_data <- T

#list of variables
varlist <- c("Emissions|CO2", "Consumption", "GDP|PPP", "Population", "Land Cover", "Land Cover|Forest", "AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile")



if(!file.exists("ar6_data.Rdata") | reload_data){
  #AR6 data
  require(reticulate)
  pyam <- import("pyam", convert = FALSE)
  
  #explore the source
  conn = pyam$iiasa$Connection('ar6-public')
  conn$models()$head()
  conn$regions()$head()
  variables <- data.frame(variable=py_to_r(conn$variables()))
  
  #load variables
  ar6_data <- pyam$read_iiasa('ar6-public', model='*', scenario="*", variable=varlist, region='World', meta=1)
  #as_pandas concatenates data and meta into a pandas DF (meta_cols = TRUE adds all meta data)
  ar6_datadf <- ar6_data$as_pandas(meta_cols = c("Ssp_family", "Policy_category", "Policy_category_name", "Category_FaIRv1.6.2", "Category", "Vetting_future", "Vetting_historical", "IMP_marker"))
  #pandas to R data frame
  ar6_datadf <- py_to_r(ar6_datadf)
  #write final data frame as Rdata file
  save(ar6_datadf, variables, file = "ar6_data.Rdata")
}else{
  load("ar6_data.Rdata")
}


#some diagnostics plots
print(ggplot(ar6_datadf %>% filter(Vetting_future=="Pass" & Vetting_historical=="Pass")) + geom_line(aes(year, value, group=interaction(scenario, model), color=variable), alpha=0.7) + facet_wrap(variable ~ ., scales = "free") + theme(legend.position = "bottom"))



                        