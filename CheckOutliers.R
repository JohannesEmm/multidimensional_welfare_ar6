min_df=tapply(X=indicators$value_pc, INDEX=indicators$variable, FUN=min, na.rm=TRUE )
write_min=as.data.frame(min_df)
write.xlsx(write_min, 'min.xlsx')

max_df=tapply(X=indicators$value_pc, INDEX=indicators$variable, FUN=max, na.rm=TRUE )
write_max=as.data.frame(max_df)
write.xlsx(write_max, 'max.xlsx')


# show all outlier data
#1) Outliers for population
test_red=filter(ar6_datadf, variable=="Population")
ggplot(test_red, aes(y=value, x=model))+geom_point()+scale_y_log10()
#--> outliers clearly above value of 10^5 (only 2 model versions and 4 scenarios in total)

#2) Outliers for consumption
test_red=filter(indicators, variable=="Consumption")
ggplot(test_red, aes(y=value_pc, x=model))+geom_point()
#--> there may be outliers below 3 thousand USD here, this is all the scenarios that the model
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
ggplot(test_red, aes(y=value_pc, x=model))+geom_point()
#--> no clear outliers

#7) Outliers for food demand
test_red=filter(indicators, variable=="Food Demand")
ggplot(test_red, aes(y=value, x=model))+geom_point()+scale_y_log10()
#--> outliers clearly above  10000 (many model versions and scenarios)

#8) Outliers for Food energy supply
test_red=filter(indicators, variable=="Food Energy Supply" )
ggplot(test_red, aes(y=value, x=model))+geom_point()
#-->  clearly outlier at 0 (IMAGE 3.2 model for a couple of scenarios)

#9) Outliers for GDP
test_red=filter(indicators, variable=="GDP|PPP")
ggplot(test_red, aes(y=value_pc, x=model))+geom_point()
#-->  no clear outliers

#10) Outliers for Land Cover
test_red=filter(indicators, variable=="Land Cover")
ggplot(test_red, aes(y=value_pc, x=model))+geom_point()
#-->  Since land cover is clearly avove 10000 million ha in the 2000s, 
# there appear to be 3 outliers below this value (2 models with multiple scenarios)

#11) Outliers for Land Cover| Forest
test_red=filter(indicators, variable=="Land Cover|Forest")
ggplot(test_red, aes(y=value_pc, x=model))+geom_point()
#-->  clearly, there are outliers for one model below 2000 (1 model)





