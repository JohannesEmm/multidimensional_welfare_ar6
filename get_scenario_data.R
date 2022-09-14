require(data.table)
require(tidyverse)

reload_data <- F

#list of variables
varlist <- c("Emissions|CO2", "Consumption", "Population", "Land Cover|Forest")



if(!file.exists("ar6_data.Rdata") & !reload_data){
  #AR6 data
  require(reticulate)
  pythondir <- "C:\\Users\\Emmerling\\AppData\\Local\\Programs\\Python\\Python310\\python.exe"
  use_python(python = pythondir, required = TRUE)
  # py_config()
  pyam <- import("pyam", convert = FALSE)
  
  #explore the source
  conn = pyam$iiasa$Connection('ar6-public')
  conn$models()$head()
  conn$regions()$head()
  variables <- data.frame(variable=py_to_r(conn$variables()))
  
  #load variables
  ar6_data <- pyam$read_iiasa('ar6-public', model='*', scenario="*", variable=varlist, region='World', meta=1)
  ar6_data <- py_to_r(ar6_data$data)
  ipcc_category <- py_to_r(ar6_data$meta$Category)
  ipcc_category <- data.frame(scenario=trimws(names(ipcc_category)), category=unlist(ipcc_category))
  #ar6_dataasiadf %>% left_join(ipcc_category)
  
  #write final data frame as Rdata file
  save(ar6_data, file = "ar6_data.Rdata")
}else{
  load("ar6_data.Rdata")
}


#some diagnostics plots
print(ggplot(ar6_data) + geom_line(aes(year, value, group=interaction(scenario, model), color=variable), alpha=0.7) + facet_wrap(variable ~ ., scales = "free"))



                        