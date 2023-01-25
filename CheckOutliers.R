min_df=tapply(X=indicators$value_pc, INDEX=indicators$variable, FUN=min, na.rm=TRUE )
write_min=as.data.frame(min_df)
write.xlsx(write_min, 'min.xlsx')

max_df=tapply(X=indicators$value_pc, INDEX=indicators$variable, FUN=max, na.rm=TRUE )
write_max=as.data.frame(max_df)
write.xlsx(write_max, 'max.xlsx')

##############################################################################################
############################## outliers global data fram #####################################
##############################################################################################

#1) Outliers for population
test_red=filter(ar6_datadf, variable=="Population")
ggplot(test_red, aes(y=value, x=model))+geom_point()+scale_y_log10()
#--> outliers clearly above value of 10^5 (only 2 model versions and 4 scenarios in total)

#2) Outliers for consumption
test_red=filter(indicators, variable=="Consumption")
ggplot(test_red, aes(y=value_pc, x=model))+geom_point()
#-->  outliers below 0.1
#--> there may be additional outliers below 3 thousand USD here, this is all the scenarios that the model
# En-ROADS-96 provided, I would not remove them

#3) Outliers for CO2 emissions
test_red=filter(indicators, variable=="Emissions|CO2")
ggplot(test_red, aes(y=value_pc, x=model))+geom_point()
#--> no clear outliers

#4) Outliers for NOx emissions
test_red=filter(indicators, variable=="Emissions|NOx")
ggplot(test_red, aes(y=value_pc, x=model))+geom_point()
#--> no clear outliers

#5) Outliers for SOx emissions
test_red=filter(indicators, variable=="Emissions|Sulfur")
ggplot(test_red, aes(y=value_pc, x=model))+geom_point()
#--> no clear outliers

#6) Outliers for Final Energy|Electricity
test_red=filter(indicators, variable=="Final Energy|Electricity")
ggplot(test_red, aes(y=value_pc, x=model))+geom_point()+scale_y_log10()
#--> clear outliers below 10^(-4)

#7) Outliers for food demand
test_red=filter(indicators, variable=="Food Demand")
ggplot(test_red, aes(y=value, x=model))+geom_point()+scale_y_log10()
#--> outliers clearly above  10000 (many model versions and scenarios)

#8) Outliers for Food energy supply
test_red=filter(indicators, variable=="Food Energy Supply" )
ggplot(test_red, aes(y=value_pc, x=model))+geom_point()
#-->  clearly outlier at 0 (IMAGE 3.2 model for a couple of scenarios)

#9) Outliers for GDP
test_red=filter(indicators, variable=="GDP|PPP" & value_pc>0.1)
ggplot(test_red, aes(y=value_pc, x=model))+geom_point()
#-->  outliers below pc value of 0.1

#10) Outliers for Land Cover
test_red=filter(indicators, variable=="Land Cover")
ggplot(test_red, aes(y=value_pc, x=model))+geom_point()
#-->  Since land cover is clearly avove 10000 million ha in the 2000s, 
# there appear to be 3 outliers below this value (2 models with multiple scenarios)

#11) Outliers for Land Cover| Forest
test_red=filter(indicators, variable=="Land Cover|Forest")
ggplot(test_red, aes(y=value_pc, x=model))+geom_point()
#-->  clearly, there are outliers for one model below 2000 (1 model)


##############################################################################################
############################## outliers regional data frame ##################################
##############################################################################################
load('ar6_data_regional.Rdata')
ar6_data_regional <- ar6_data_regional %>% left_join(ar6_data_regional %>% filter(variable=="Population") %>% select(-variable,-unit) %>% rename(Population=value))
ar6_data_regional <- ar6_data_regional %>% mutate(valuepc=value/Population)

#1) Outliers for population
test_red=filter(ar6_data_regional, variable=="Population")
ggplot(test_red, aes(y=value, x=model, color=region))+geom_point()+scale_y_log10()
test_red=filter(ar6_data_regional, variable=="Population" & value<10^5)
ggplot(test_red, aes(y=value, x=model, color=region))+geom_point()+scale_y_log10()
#--> outliers clearly above value of 10^5 (one model with multiple scenarios)
#--> there may be some more outliers around the value of 10 but not so clear so leave

#2) Outliers for consumption
test_red=filter(ar6_data_regional, variable=="Consumption")
ggplot(test_red, aes(y=valuepc, x=model, color=region))+geom_point()+scale_y_log10()
test_red=filter(ar6_data_regional, variable=="Consumption" & valuepc>0.1)
ggplot(test_red, aes(y=valuepc, x=model, color=region))+geom_point()+scale_y_log10()
#-->  outliers below 0.1

#3) Outliers for CO2 emissions
test_red=filter(ar6_data_regional, variable=="Emissions|CO2")
ggplot(test_red, aes(y=valuepc, x=model, color=region))+geom_point()
#--> no clear outliers

#4) Outliers for NOx emissions
test_red=filter(ar6_data_regional, variable=="Emissions|NOx")
ggplot(test_red, aes(y=valuepc, x=model, color=region))+geom_point()+scale_y_log10()
#--> no clear outliers

#5) Outliers for SOx emissions
test_red=filter(ar6_data_regional, variable=="Emissions|Sulfur")
ggplot(test_red, aes(y=valuepc, x=model, color=region))+geom_point()+scale_y_log10()
#--> no clear outliers

#6) Outliers for Final Energy|Electricity
test_red=filter(ar6_data_regional, variable=="Final Energy|Electricity")
ggplot(test_red, aes(y=valuepc, x=model, color=region))+geom_point()+scale_y_log10()
test_red=filter(ar6_data_regional, variable=="Final Energy|Electricity" & valuepc<2*10^(-4))
ggplot(test_red, aes(y=valuepc, x=model, color=region))+geom_point()+scale_y_log10()
#--> outliers below 2*10^(-4) (one model with many scenarios)

#7) Outliers for food demand
test_red=filter(ar6_data_regional, variable=="Food Demand")
ggplot(test_red, aes(y=value, x=model, color=region))+geom_point()+scale_y_log10()
test_red1=filter(ar6_data_regional, variable=="Food Demand" & value<5000)
ggplot(test_red1, aes(y=value, x=model, color=region))+geom_point()+scale_y_log10()
#--> set outliers above 5000

#8) Outliers for Food energy supply
test_red=filter(ar6_data_regional, variable=="Food Energy Supply" )
ggplot(test_red, aes(y=valuepc, x=model, color=region))+geom_point()
test_red=filter(ar6_data_regional, variable=="Food Energy Supply" & valuepc>0.001)
ggplot(test_red, aes(y=valuepc, x=model, color=region))+geom_point()
#-->  clearly outlier at below 0.001 (IMAGE 3.2 model)

#9) Outliers for GDP
test_red=filter(ar6_data_regional, variable=="GDP|PPP" )
ggplot(test_red, aes(y=valuepc, x=model, color=region))+geom_point()+scale_y_log10()
test_red=filter(ar6_data_regional, variable=="GDP|PPP" & valuepc>0.2 )
ggplot(test_red, aes(y=valuepc, x=model, color=region))+geom_point()+scale_y_log10()
#-->  outliers below pc value of 0.2 (two models with multiple scenarios)

#10) Outliers for Land Cover
test_red=filter(ar6_data_regional, variable=="Land Cover")
ggplot(test_red, aes(y=value, x=model, color=region))+geom_point()
#-->  no clear outliers

#11) Outliers for Land Cover| Forest
test_red=filter(ar6_data_regional, variable=="Land Cover|Forest")
ggplot(test_red, aes(y=value, x=model, color=region))+geom_point()
#-->  no clear outliers



ar6_data_regional <- ar6_data_regional %>% filter(Population < 1*10^5 & !(variable=="Food Demand" & value>5000) & !(variable=="Final Energy|Electricity" & valuepc <2*10^(-4))  & !(variable=="Consumption" & valuepc <0.1) & !(variable=="GDP|PPP" & valuepc <0.2)& !(variable=="Food Energy Supply" & valuepc <0.001)  )
#get extrema
ar6_data_regional_extremes_pc <- ar6_data_regional %>% group_by(variable) %>% summarize(min=min(valuepc, na.rm = T), max=max(valuepc, na.rm = T))
print(ar6_data_regional_extremes_pc)

ar6_data_regional_extremes <- ar6_data_regional %>% group_by(variable) %>% summarize(min=min(value, na.rm = T), max=max(value, na.rm = T))
print(ar6_data_regional_extremes)





