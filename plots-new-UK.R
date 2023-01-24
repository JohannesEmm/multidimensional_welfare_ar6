# this plots the welfare data, run 'main.R' first
# saves in subfolder "figures"

library("RColorBrewer")


dir.create("figures")


  
###########################################################################################
############### plot variables ################
###########################################################################################
  # all scenarios
  data_m <- indicators %>%
    filter(variable !="Consumption")%>%
    filter(variable !="Emissions|CO2")%>%
    filter(variable !="Food Demand")%>%
    filter(variable !="Land Cover")%>%
    dplyr::filter(Category!="failed-vetting")
  
  data_m$variable[data_m$variable == "AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile"]="Temperature [K]"
  data_m$variable[data_m$variable == "Final Energy|Electricity"]="Energy|Electricity [EJ/yr]"
  data_m$variable[data_m$variable == "Emissions|NOx"]="Emissions|NOx [Mt NO2/yr]"
  data_m$variable[data_m$variable == "Emissions|Sulfur"]="Emissions|Sulfur [Mt SO2/yr]"
  data_m$variable[data_m$variable == "GDP|PPP"]="GDP|PPP [billion Int$2010/yr]"
  data_m$variable[data_m$variable == "Population"]="Population [Million]"
  data_m$variable[data_m$variable == "Food Energy Supply"]="Food Energy Supply [EJ/yr]"
  data_m$variable[data_m$variable == "Land Cover|Forest"]="Land Cover|Forest [million ha]"
  
  theme_set(theme_bw())
  png(file = paste("figures/","AR6_database- variables",".png",sep=""), width = 12000, height = 12000, units = "px") 
  
  p=(ggplot(data_m ) +
       geom_line(aes(year, value, group=interaction(model,scenario)), alpha = 0.7) + 
       labs(title = paste("AR6-database:", "variables for welfare metric"),
            y = "", x = "") + 
       scale_x_continuous(breaks = seq(from = 2000, to = 2099, by = 50))+
       facet_wrap( ~ variable, scales = "free", ncol=4)+ theme(text = element_text(size = 200)))
  print(p)
  dev.off()  
  
###########################################################################################
##################################prepare data ##################################
###########################################################################################
  
  # plot indicators
  # plot only those indicators of scenarios for which welfare could be computed:
  identifier_welfares <- paste(welfares$model, welfares$scenario, welfares$year)
  indicators$identifier<- paste(indicators$model, indicators$scenario, indicators$year)
  indicators$b_welfare<- rep(0, dim(indicators)[1])
  indicators$b_welfare[indicators$identifier %in% identifier_welfares]=1
  
  #remove all non-relevant scenarios
  data_plot= indicators %>%
    filter(b_welfare>0) %>%
    filter(variable !="Consumption")%>%
    filter(variable !="Emissions|CO2")%>%
    filter(variable !="Food Demand")%>%
    filter(variable !="Land Cover")%>%
    filter(variable !="Population")%>%
    dplyr::filter(Category!="failed-vetting")
  #dplyr::filter(Category!="C8") #removing C8 because it is only one scenario in here
  
  # ammend the welfare to data_plot
  #pick which welfare parameters to choose:
  welfare_plot= welfares %>%
    filter(rho=="1" & weights=="1,0,0,0.5,0.5,1,1,0,1,0,1,0") #equal weights and some substitutability
  #for that remove non matching columns
  welfare_plot$variable="Welfare"
  welfare_plot$rho<- NULL
  welfare_plot$weights<-NULL
  welfare_plot$indicator<-welfare_plot$value
  
  indicators_plot=data_plot
  indicators_plot$b_welfare<-NULL
  indicators_plot$log<-NULL
  indicators_plot$bad<-NULL
  indicators_plot$min<-NULL
  indicators_plot$max<-NULL
  indicators_plot$value_pc<-NULL
  indicators_plot$identifier<-NULL
  #rename some variables for better plotting
  indicators_plot$variable[indicators_plot$variable == "AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile"]="Temperature"
  indicators_plot$variable[indicators_plot$variable == "Final Energy|Electricity"]="Energy|Electricity"
  
  data_plot=rbind(indicators_plot,welfare_plot)
  
  data_plot= data_plot%>%
    dplyr::filter(Category!="failed-vetting")
  #df_test=filter(indicators, Category=="C1" & variable=="GDP|PPP" & b_welfare >0 & year=="2100")
  #C1 has 13 scenarios
  #C2 has 25 scenarios
  #C3 has 60 scenarios
  #C4 has 31 scenarios
  #C5 has 43 scenarios
  #C6 has 18 scenarios
  #C7 has 24 scenarios
  #C8 has 1 scenario
  
###########################################################################################
############### plot indicators and welfare with jitter for selected years################
###########################################################################################

  data_plot_red= data_plot %>%
    filter(year %in% c(2030,2060,2100))
  data_plot_red$yearcat=as.factor(paste(data_plot_red$year, data_plot_red$Category))
  
  data_m <- data_plot %>% group_by(Category, variable, year) %>%
    dplyr::summarize(Mean = mean(indicator, na.rm=TRUE), SD=sd(indicator, na.rm=TRUE))
  data_m<- data_m %>% 
    drop_na(Mean, SD)
  data_m <- data_m %>% 
    filter(year %in% c(2030,2060,2100))
  data_m$yearcat=as.factor(paste(data_m$year, data_m$Category))
  
    
theme_set(theme_bw())
png(file = paste("figures/","AR6_database - indicators",".png",sep=""), width = 1200, height = 1200, units = "px") 
p=(ggplot(data_plot_red,aes(x=yearcat, indicator, group=interaction(year,Category))) +
  geom_jitter(aes(color=Category, group=interaction(year,Category)),width = 0.1, cex=2) + 
  labs(title = paste("AR6-database: ", "Indicators and welfare"),
       y = "", x = "") + 
    scale_x_discrete(breaks = c("2030 C4","2060 C4","2100 C4"), labels=c("2030","2060","2100"))+
  facet_wrap(~variable , scales = "free", ncol=4)+
    scale_color_brewer(palette="BrBG")+#geom_point(data=data_m, shape=7, aes(x=yearcat, y=Mean, group = Category, colour=Category),size=2)+ # here you can see that the distribution does not really deliver a meaningful mean
    scale_fill_brewer(palette="BrBG")+ theme(text = element_text(size = 28))  )
print(p)
dev.off()

#########################################################################################
############### plot indicators and welfare with geomline for all years ################
###########################################################################################

theme_set(theme_bw())
data_m <- data_plot %>% group_by(Category, variable, year) %>%
  dplyr::summarize(Mean = mean(indicator, na.rm=TRUE), SD=sd(indicator, na.rm=TRUE))
data_m<- data_m %>% 
  drop_na(Mean, SD)
data_m <- data_m %>% 
  filter(year %in% seq(from = 2010, to = 2100, by = 10))

png(file = paste("figures/","AR6 database - mean indicators",".png",sep=""), width = 1200, height = 1200, units = "px") 
p=(ggplot(data_m, aes(x = year, group = Category)) +
     geom_line(aes(y=Mean, color=Category), size=1) + 
     geom_ribbon(aes(y = Mean, ymin = Mean - SD, ymax = Mean + SD, fill = Category), alpha = .1)+
     labs(title = paste("AR6-database:", "mean indicators and welfare"),
          y = "", x = "") + 
     scale_x_continuous(breaks = seq(from = 2020, to = 2099, by = 30))+
     facet_wrap( ~ variable , scales = "free", ncol=4)+
     scale_color_brewer(palette="BrBG")+
     scale_fill_brewer(palette="BrBG")+ theme(text = element_text(size = 28)))
print(p)
dev.off()

#########################################################################################
##################################### plot welfares means ####################################
###########################################################################################

#subplot per rho and weight
weight.labs <- c("weight:high GDP", "weight:equal", "weight:low GDP")
names(weight.labs) <- c(paste(weights[1][[1]], sep=" ", collapse=","), paste(weights[2][[1]], sep=" ", collapse=","), paste(weights[3][[1]], sep=" ", collapse=","))
rho.labs <- c("rho:0", "rho:1", "rho:5")
names(rho.labs) <- c("0", "1", "5")




theme_set(theme_bw())
png(file = paste("figures/","AR6_database mean welfares",".png",sep=""), width = 1200, height = 1200, units = "px") 
data_m <- welfares %>% group_by(Category, year, rho, weights) %>%
  dplyr::summarize(Mean = mean(value, na.rm=TRUE), SD=sd(value, na.rm=TRUE))
data_m<- data_m %>% 
  drop_na(Mean, SD)
data_m <- data_m %>% 
  filter(year %in% seq(from = 2010, to = 2100, by = 10))%>%
  dplyr::filter(Category!="failed-vetting")
p=(ggplot(data_m, aes(x = year, group = Category)) +
     geom_line(aes(y=Mean, color=Category), size=1) + 
     geom_ribbon(aes(y = Mean, ymin = Mean - SD, ymax = Mean + SD, fill = Category), alpha = .1)+
     labs(title = paste("AR6-database:", "welfare metric by rho and weight"),
          y = "welfare", x = "") + 
     scale_x_continuous(breaks = seq(from = 2000, to = 2099, by = 50))+
     ggh4x::facet_grid2( rho ~ weights, scales = "free_y", independent = "y", labeller=labeller(rho = rho.labs, weights = weight.labs))+
     scale_color_brewer(palette="BrBG")+
     scale_fill_brewer(palette="BrBG")+ theme(text = element_text(size = 28)))
print(p)
dev.off()


#########################################################################################
##################################### plot welfare for selected years ####################################
###########################################################################################

#subplot per rho and weight
weight.labs <- c("weight:high GDP", "weight:equal", "weight:low GDP")
names(weight.labs) <- c(paste(weights[1][[1]], sep=" ", collapse=","), paste(weights[2][[1]], sep=" ", collapse=","), paste(weights[3][[1]], sep=" ", collapse=","))
rho.labs <- c("rho:0", "rho:1", "rho:5")
names(rho.labs) <- c("0", "1", "5")

welf_plot_red= welfares %>%
  filter(year %in% c(2030,2060,2100))
welf_plot_red$yearcat=as.factor(paste(welf_plot_red$year, welf_plot_red$Category))

data_m <- welf_plot_red%>%
  dplyr::filter(Category!="failed-vetting")

theme_set(theme_bw())
png(file = paste("figures/","AR6_database welfares",".png",sep=""), width = 1200, height = 1200, units = "px") 
p=(ggplot(data_m,aes(x=yearcat, value, group=interaction(year,Category))) +
     geom_jitter(aes(color=Category, group=interaction(year,Category)),width = 0.1, cex=2) + 
     labs(title = paste("AR6-database:", "welfare metric by rho and weight"),
          y = "", x = "")  + 
     scale_x_discrete(breaks = c("2030 C4","2060 C4","2100 C4"), labels=c("2030","2060","2100"))+
     ggh4x::facet_grid2( rho ~ weights, scales = "free_y", independent = "y", labeller=labeller(rho = rho.labs, weights = weight.labs))+
     scale_color_brewer(palette="BrBG")+#geom_point(data=data_m, shape=7, aes(x=yearcat, y=Mean, group = Category, colour=Category),size=2)+ # here you can see that the distribution does not really deliver a meaningful mean
     scale_fill_brewer(palette="BrBG")+ theme(text = element_text(size = 28))  )
print(p)
dev.off()




