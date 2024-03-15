######### create all plots for the AR6 database multidimensional analysis, save in folder "figures" ########

if(!dir.exists("figures")){dir.create("figures")}


# Labels
weight.labs <- c("weight:high GDP", "weight:equal", "weight:low GDP")
names(weight.labs) <- c(paste(weights[1][[1]], sep=" ", collapse=","), paste(weights[2][[1]], sep=" ", collapse=","), paste(weights[3][[1]], sep=" ", collapse=","))
rho.labs <- c("rho:0", "rho:1", "rho:5")
names(rho.labs) <- c("0", "1", "5")


#####################################################################
theme_set(theme_bw())


combined_data <- welfares %>% select(-IMP_marker) %>% filter(!is.na(selected_weights)) %>% left_join(ar6_datadf %>% select(-unit) %>% pivot_wider(names_from = variable, values_from = "value") %>% mutate(gdppc=`GDP|PPP`/Population*1000)) %>% filter(Category!="C8")

combined_data$weightname <- plyr::mapvalues(combined_data$selected_weights, from = c("1, 0, 0, 0.5, 0.5, 1, 0.01, 0, 1, 0, 1, 0" , "0.01, 0, 0, 0.005, 0.005, 0.01, 0.01, 0, 0.01, 0, 0.01, 0", "0.01, 0, 0, 0.005, 0.005, 0.01, 1, 0, 0.01, 0, 0.01, 0" ), to = c("weight:low GDP", "weight:equal", "weight:high GDP"))
combined_data$weightname = factor(combined_data$weightname, levels=c("weight:low GDP", "weight:equal", "weight:high GDP"), labels=c("weight:low GDP", "weight:equal", "weight:high GDP")) 

ggplot(combined_data %>% filter(year %in% c(2050, 2100) & Category!="failed-vetting")) + geom_point(aes(gdppc, value, color=Category)) + facet_wrap(year ~ .) + labs(x="GDP per capita [kUSD-$(2015)/cap]", y="Welfare index") + scale_color_brewer(palette="RdYlBu", direction = -1)#+ scale_fill_brewer(palette="BrBG")
ggsave("figures/Index GDP Scatter 2050 2100.png", width=8, height = 5)


#for policy brief
ggplot(combined_data %>% filter(year %in% c(2100) & Category!="failed-vetting") %>% filter(weightname=="weight:equal" & rho==1)) + geom_point(aes(Category, value, color=Category)) + scale_color_brewer(palette="RdYlBu", direction = -1) + labs(y="Multidimensional welfare index", x = "IPCC AR6 scenario category") + guides(color="none")
#add mean value as small black lines
ggplot(combined_data %>% filter(year %in% c(2100) & Category!="failed-vetting") %>% filter(weightname=="weight:equal" & rho==1), aes(Category, value, color=Category)) + geom_point() + scale_color_brewer(palette="RdYlBu", direction = -1) + labs(y="Multidimensional welfare index", x = "IPCC AR6 scenario category") + guides(color="none") +   stat_summary(geom = "point", fun = "mean", col = "black", size = 4, shape = 4, fill = "black")
ggsave("figures/PB_welfare.png", width=8, height = 6)
ggsave("figures/PB_welfare.pdf", width=6, height = 4)


year_for_reg_line <- c(2100)
#year_for_reg_line <- c(2050)
#year_for_reg_line <- c(2030)
#year_for_reg_line <- c(2060)
#year_for_reg_line <- seq(2025,2100,5)

ggplot(combined_data %>% filter(year %in% year_for_reg_line & Category!="failed-vetting")) + geom_point(aes(gdppc, value, color=Category)) + facet_grid(paste0("rho:",rho) ~ weightname) + labs(x="GDP per capita [kUSD-$(2015)/cap]", y="Welfare index") + ylim(0,1) + scale_color_brewer(palette="RdYlBu", direction = -1)+ scale_fill_brewer(palette="BrBG")
ggsave(str_glue("figures/Index_GDP_Scatter_for_rho_{year_for_reg_line[1]}.png"), width=8, height = 5)



#add regression line TEMP
ggplot(combined_data %>% filter(year %in% year_for_reg_line & Category!="failed-vetting")) + geom_point(aes(`AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile`, value, color=Category)) + facet_grid(paste0("rho:",rho) ~ weightname) + labs(x="Temperature increase", y="Welfare index", color="Category") + theme(legend.position="bottom") + geom_smooth(aes(`AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile`, value), color="black", method="lm")  + ylim(0,1)  + scale_color_brewer(palette="RdYlBu", direction = -1)  + theme(legend.position = "right")
ggsave(str_glue("figures/Temp_Welfare_Index_{year_for_reg_line[1]}.png"), width=8, height = 5)

combined_data <- combined_data %>% mutate(gdppc=`GDP|PPP`/Population)
#coefficients
coefficients_temp_welfare <- combined_data %>% filter(year %in% year_for_reg_line & Category!="failed-vetting") %>% nest_by(rho, weightname) %>% mutate(reg = list(lm(value ~ `AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile`, data = data))) %>% reframe(broom::tidy(reg)) %>% filter(term!="(Intercept)")
openxlsx::write.xlsx(coefficients_temp_welfare, file="figures/reg_temp.xlsx")


#add regression line GDP
ggplot(combined_data %>% filter(year %in% year_for_reg_line & Category!="failed-vetting")) + geom_point(aes(gdppc, value, color=Category)) + facet_grid(paste0("rho:",rho) ~ weightname) + labs(x="GDP per capita [kUSD-$(PPP)/cap]", y="Welfare index", color="Category") + theme(legend.position="bottom") + geom_smooth(aes(gdppc, value), color="black", method="lm")  + ylim(0,1)  + scale_color_brewer(palette="RdYlBu", direction = -1) + theme(legend.position = "right")
ggsave(str_glue("figures/GDP_Welfare_Index_{year_for_reg_line[1]}.png"), width=8, height = 5)
#coefficients
coefficients_temp_welfare <- combined_data %>% filter(year %in% year_for_reg_line & Category!="failed-vetting") %>% nest_by(rho, weightname) %>% mutate(reg = list(lm(value ~ gdppc, data = data))) %>% reframe(broom::tidy(reg)) %>% filter(term!="(Intercept)")
openxlsx::write.xlsx(coefficients_temp_welfare, file="figures/reg_gdp.xlsx")



year_for_reg_line <- c(2030,2060,2100)

#now all weights
combined_data_allweights <- welfares %>% select(-IMP_marker) %>% left_join(ar6_datadf %>% select(-unit) %>% pivot_wider(names_from = variable, values_from = "value") %>% mutate(gdppc=`GDP|PPP`/Population*1000)) %>% filter(Category!="C8")
combined_data_allweights <- combined_data_allweights %>% mutate(sum_of_weights = across(starts_with("weight_")) %>% rowSums) %>% filter(year %in% year_for_reg_line & Category!="failed-vetting") 

#GDPPC
coefficients_gdppc_welfare_allweights <- combined_data_allweights %>% filter(year %in% year_for_reg_line & Category!="failed-vetting") %>% nest_by(rho, weights) %>% mutate(reg = list(lm(value ~ gdppc, data = data))) %>% reframe(broom::tidy(reg))
#Temperature
coefficients_temp_welfare_allweights <- combined_data_allweights %>% filter(year %in% year_for_reg_line & Category!="failed-vetting") %>% nest_by(rho, weights) %>% mutate(reg = list(lm(value ~ `AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile`, data = data))) %>% reframe(broom::tidy(reg))

for(.temp in year_for_reg_line){
#now loop for all variables
p_violin <- list()
for(var in paste0("`",str_subset(names(combined_data_allweights), pattern = "weight_"),"`")[c(1,4,5,6,7,9,11)]){
  print(gsub("`", "", gsub("weight_","",var)))
  #get scenario dataframe of specific normalized weights
  combined_data_allweights_onlyweights <- NULL
  combined_data_allweights_onlyweights <- copy(combined_data_allweights)
  combined_data_allweights_onlyweights <- combined_data_allweights_onlyweights %>% filter(year==.temp)
  #Temperature regression
  coefficients_temp_welfare_allweights <- combined_data_allweights_onlyweights %>% filter(year %in% year_for_reg_line & Category!="failed-vetting") %>% nest_by(rho, weights) %>% mutate(reg = list(lm(value ~ `AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile`, data = data))) %>% reframe(broom::tidy(reg))
  #plots
  setnames(combined_data_allweights_onlyweights, gsub("`", "", var), "temp_weight")
  combined_data_allweights_onlyweights <- combined_data_allweights_onlyweights %>% ungroup() %>% mutate(weight_report = temp_weight/sum_of_weights) %>% group_by(rho, weights) %>% dplyr::summarise(weight_report = mean(weight_report))
  varnice <-suppressWarnings(plyr::mapvalues(var, from = paste0("`",str_subset(names(combined_data_allweights), pattern = "weight_"),"`")[c(1,4,5,6,7,9,11)], to = c("Temperature", "NOx Emissions", "Sulfur Emissions", "Electricity", "GDP", "Forest Cover", "Food Supply")))
  p_violin[[varnice]] <- ggplot(coefficients_temp_welfare_allweights %>% filter(term=="`AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile`") %>% left_join(combined_data_allweights_onlyweights), aes(x = as.factor(rho), y = estimate, color=weight_report)) + geom_violin(draw_quantiles = c(0.5), alpha=0.6, color = "black") +
  labs(x=TeX("$\\rho$"), y="", title = varnice, color ="Weight") + geom_jitter(position = position_jitter(seed = 1, width = 0.1), alpha = 0.4) + geom_hline(yintercept = 0) + theme(legend.position = "bottom") + scale_colour_gradient(low = "yellow", high = "blue", limits = c(0,1))
  ggsave(gsub(" ", "-", gsub("\\|", "-", str_glue('figures/violin_{gsub("`", "", gsub("weight_","",var))}_{year_for_reg_line[1]}.png'))))
}
p_arranged <- ggarrange(p_violin$Temperature, p_violin$GDP, p_violin$`NOx Emissions`, p_violin$`Sulfur Emissions`, p_violin$Electricity, p_violin$`Forest Cover`, p_violin$`Food Supply`, nrow = 4, ncol = 2, common.legend = T, legend = "bottom")
p_arranged <- annotate_figure(p_arranged, left = text_grob("Slope of the Welfare - Temperature relationship (per 1°C)", color = "black", rot = 90))
ggsave(str_glue("figures/arranged_violin_plot_{.temp}.pdf"), width = 8, height = 12)

#just a Food Supply plot for the main paper
ggplot(coefficients_temp_welfare_allweights %>% filter(term=="`AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile`") %>% left_join(combined_data_allweights_onlyweights), aes(x = as.factor(rho), y = estimate, color=weight_report)) + geom_violin(draw_quantiles = c(0.5), alpha=0.6, color = "black") +
  labs(x=TeX("$\\rho$"), y="change per 1°C", title = "Slope of the Welfare - Temperature relationship", color ="relative weight of Food Supply") + geom_jitter(position = position_jitter(seed = 1, width = 0.1), alpha = 0.4) + geom_hline(yintercept = 0) + theme(legend.position = "bottom") + scale_colour_gradient(low = "yellow", high = "blue", limits = c(0,1))
ggsave(str_glue("figures/violin_plot_food_supply_for_main_paper_{.temp}.pdf"))
}


#some statistics for the interpretation
combined_data
mean(combined_data$value)
sd(combined_data$value)











###########################################################################################
############### plot variables ################
###########################################################################################
# all scenarios
data_m <- indicators %>%
  filter(variable !="Consumption")%>%
  filter(variable !="Emissions|CO2")%>%
  filter(variable !="Food Demand")%>%
  filter(variable !="Land Cover")%>%
  dplyr::filter(Category!="failed-vetting")%>%
  dplyr::filter(Category!="no-climate-assessment")%>%
  dplyr::filter(Category!="NaN")

data_m$variable[data_m$variable == "Temperature"]="Temperature [K]"
data_m$variable[data_m$variable == "Electricity"]="Electricity [EJ/yr]"
data_m$variable[data_m$variable == "NOx Emissions"]="NOx Emissions [Mt NO2/yr]"
data_m$variable[data_m$variable == "Sulfur Emissions"]="Sulfur Emissions [Mt SO2/yr]"
data_m$variable[data_m$variable == "GDP"]="GDP [billion Int$2010/yr]"
data_m$variable[data_m$variable == "Population"]="Population [Million]"
data_m$variable[data_m$variable == "Food Supply"]="Food Supply [EJ/yr]"
data_m$variable[data_m$variable == "Forest Cover"]="Forest Cover [million ha]"

theme_set(theme_bw())

ggplot(data_m) +
  geom_line(aes(x=year, y=value, color=Category, group=interaction(model,scenario)), linewidth=0.1) + 
  labs(title = paste("AR6-database:", "variables for welfare metric"),
       y = "", x = "Time") + 
  scale_x_continuous(breaks = seq(from = 1990, to = 2100, by = 50))+
  facet_wrap( ~ variable, scales = "free", ncol=4)+ scale_color_brewer(palette="RdYlBu", direction = -1)#+ theme(text = element_text(size = 200))
ggsave("figures/AR6_database- variables.pdf", width=10, height = 6)


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

# ammend the welfare to data_plot
welfares_plot=welfares
# pick which welfare parameters to choose:
welfare_plot= welfares_plot %>%
  filter(rho==1 & weights=="0.01, 0, 0, 0.005, 0.005, 0.01, 0.01, 0, 0.01, 0, 0.01, 0") #equal weights and some substitutability
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

data_plot=bind_rows(indicators_plot,welfare_plot)

data_plot= data_plot%>%
  dplyr::filter(Category!="failed-vetting")

# how many scenarios in models in each C-category?
df_test=filter(welfare_plot, Category=="C8"& year=="2030" )
# in  2030,2060,2100:
#C1 has 13 scenarios
#C2 has 21 scenarios (25 with all SSPs)
#C3 has 56 scenarios (60 with all SSPs)
#C4 has 31 scenarios
#C5 has 39 scenarios (43 with all SSPs)
#C6 has 18 scenarios
#C7 has 20 scenarios (24 with all SSPs)

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
#png(file = paste("figures/","AR6_database - indicators",".png",sep=""), width = 1200, height = 1200, units = "px") 
ggplot(data_plot_red,aes(x=yearcat, indicator, group=interaction(year,Category))) +
  theme_bw() + 
  geom_rect(xmin = 7.5, xmax = 14.5, ymin = -0.5, ymax = 1.5,
            fill = 'snow2', alpha = 0.05) +
  geom_jitter(aes(color=Category, group=interaction(year,Category)),width = 0.1, size=0.5) +  
  labs(title = paste("AR6 database:", "Indicators and welfare"),
       y = "", x = "") + 
  scale_x_discrete(breaks = c("2030 C4","2060 C4","2100 C4"), labels=c("2030","2060","2100"),expand=c(0.05, 0))+
  facet_wrap(~variable , scales = "free", ncol=4)+#geom_point(data=data_m, shape=7, aes(x=yearcat, y=Mean, group = Category, colour=Category),size=2)+ # here you can see that the distribution does not really deliver a meaningful mean
  scale_color_brewer(palette="RdYlBu", direction = -1)#+ theme(text = element_text(size = 28))  
ggsave("figures/AR6_database - indicators.pdf", width=10, height = 6)

###########################################################################################
################################## prepare plots for 1 welfare and indicators##############
###########################################################################################

# ammend the welfare to data_plot
welfares_plot=welfares
#pick which welfare parameters to choose:
welfare_plot= welfares_plot %>%
  filter(rho==1 & weights=="0.01, 0, 0, 0.005, 0.005, 0.01, 0.01, 0, 0.01, 0, 0.01, 0") #equal weights and some substitutability

welfare_base=welfare_plot
welfare_base <- filter(welfare_base, year != "", value != "", Category != "failed-vetting") %>%
  dplyr:: filter(!is.na(value)) %>% 
  filter(year %in% seq(from = 2010, to = 2100, by = 10)) 
welfare_base$value<-as.numeric(welfare_base$value) 

indicators_2 <- filter(indicators_plot, year != "", indicator != "", Category != "failed-vetting") %>%
  dplyr:: filter(!is.na(indicator)) %>% 
  filter(year %in% seq(from = 2010, to = 2100, by = 10)) 
indicators_2$indicator<-as.numeric(indicators_2$indicator) 

indicators_2$variable2 <- indicators_2$variable
indicators_2 <- indicators_2 %>%
  mutate(variable2=recode(variable2, "AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile"="Temperature","Comsumption"="Comsumption","Emissions|CO2"="CO2 Emissions","Emissions|NOx"="NOx Emissions","Emissions|Sulfur"="Sulfur Emissions","Final Energy|Electricity"="Electricity","Food Demand"="Food Demand", "Food Energy Supply"="Food Supply","GDP|PPP"="GDP","Land Cover"="Land Cover","Land Cover|Forest"="Forest Cover","Population"="Population"))


### Scatter Plots

library(gridExtra)

theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank())

PWelf2060 <- ggplot(subset(welfare_base,year==2060), aes(x=Category, y=value, color=Category)) +
  geom_point(size=1)+scale_color_brewer(palette="RdYlBu", direction = -1) + 
  ggtitle("Welfare",subtitle = "2060") + coord_cartesian(ylim=c(0,1.25)) + guides(col = guide_legend(nrow = 4))

print(PWelf2060)

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend <- get_legend(PWelf2060)

PWelf2060 <- PWelf2060+ theme(legend.position="none")

for(i in c(2030,2100)) 
{                    
  assign(paste0("PWelf", i), ggplot(subset(welfare_base,year==i), aes(x=Category, y=value, color=Category)) +
           geom_point(size=1)+
           scale_color_brewer(palette="RdYlBu", direction = -1) + theme(legend.position="none") + ggtitle(" ",subtitle = paste0(i)) + coord_cartesian(ylim=c(0,1.25)))
}

varset <- unique(indicators_2$variable2)
i=2060
for(j in varset) 
{                    
  assign(paste0("P",gsub(" ", "", j),"2060"), ggplot(subset(indicators_2,year==2060&variable2==j), aes(x=Category, y=indicator, color=Category)) +
           geom_point(size=1)+
           scale_color_brewer(palette="RdYlBu", direction = -1) + theme(legend.position="none") + ggtitle(paste0(j),subtitle = paste0(i)) + coord_cartesian(ylim=c(0,1.25)))
}


for(j in varset) 
{                    
  for (i in c(2030,2100))
  {
    assign(paste0("P",gsub(" ", "", j),i), ggplot(subset(indicators_2,year==i&variable2==j), aes(x=Category, y=indicator, color=Category)) +
             geom_point(size=1)+
             scale_color_brewer(palette="RdYlBu", direction = -1) + theme(legend.position="none") + ggtitle("",subtitle = paste0(i)) + coord_cartesian(ylim=c(0,1.25)))
  }
}  

pdf(file ="figures/ScatterPlot-Cat.pdf",width=12.5,height=8,pointsize=4) 
p=grid.arrange(PTemperature2030, PTemperature2060, PTemperature2100, PGDP2030, PGDP2060, PGDP2100, legend, 
               PNOxEmissions2030, PNOxEmissions2060, PNOxEmissions2100, PSulfurEmissions2030, PSulfurEmissions2060, PSulfurEmissions2100,PForestCover2030, PForestCover2060, PForestCover2100, 
               PFoodSupply2030, PFoodSupply2060, PFoodSupply2100, PElectricity2030, PElectricity2060, PElectricity2100, PWelf2030, PWelf2060, PWelf2100, 
               ncol=9, nrow=4, widths=c(2, 2, 2, 0.3, 2, 2, 2, 0.3, 2),layout_matrix=rbind(c(1,2,3,NA,4,5,6,NA,7),
                                                                                           c(8,9,10,NA,11,12,13,NA,NA),
                                                                                           c(14,15,16,NA,17,18,19,NA,NA),
                                                                                           c(20,21,22,NA,23,24,25,NA,NA)))
print(p)
dev.off()





#########################################################################################
##################################### plot welfare for selected years ####################################
###########################################################################################

#subplot per rho and weight
weight.labs <- c("weight:high GDP", "weight:equal", "weight:low GDP")
names(weight.labs) <- c("0.01, 0, 0, 0.005, 0.005, 0.01, 1, 0, 0.01, 0, 0.01, 0", "0.01, 0, 0, 0.005, 0.005, 0.01, 0.01, 0, 0.01, 0, 0.01, 0", "1, 0, 0, 0.5, 0.5, 1, 0.01, 0, 1, 0, 1, 0")
rho.labs <- c("rho:0", "rho:1", "rho:5")
names(rho.labs) <- c("0", "1", "5")

welf_plot_red= welfares %>%
  filter(year %in% c(2030,2060,2100) & weights %in% c("0.01, 0, 0, 0.005, 0.005, 0.01, 1, 0, 0.01, 0, 0.01, 0", "0.01, 0, 0, 0.005, 0.005, 0.01, 0.01, 0, 0.01, 0, 0.01, 0", "1, 0, 0, 0.5, 0.5, 1, 0.01, 0, 1, 0, 1, 0"))
welf_plot_red$yearcat=as.factor(paste(welf_plot_red$year, welf_plot_red$Category))

data_m <- welf_plot_red%>%
  dplyr::filter(Category!="failed-vetting")

data_m$weightname <- plyr::mapvalues(data_m$selected_weights, from = c("1, 0, 0, 0.5, 0.5, 1, 0.01, 0, 1, 0, 1, 0" , "0.01, 0, 0, 0.005, 0.005, 0.01, 0.01, 0, 0.01, 0, 0.01, 0", "0.01, 0, 0, 0.005, 0.005, 0.01, 1, 0, 0.01, 0, 0.01, 0" ), to = c("weight:low GDP", "weight:equal", "weight:high GDP"))
data_m$weightname = factor(data_m$weightname, levels=c("weight:low GDP", "weight:equal", "weight:high GDP"), labels=c("weight:low GDP", "weight:equal", "weight:high GDP")) 


# add the averages to the plot:
df2 <- data_m %>% group_by(Category, year, rho, weights) %>% 
  summarise(
    Welfare=mean(value),
    .groups = 'drop') 

df2$yearcat=as.factor(paste(df2$year, df2$Category))

df2$weightname <- plyr::mapvalues(df2$weights, from = c("1, 0, 0, 0.5, 0.5, 1, 0.01, 0, 1, 0, 1, 0" , "0.01, 0, 0, 0.005, 0.005, 0.01, 0.01, 0, 0.01, 0, 0.01, 0", "0.01, 0, 0, 0.005, 0.005, 0.01, 1, 0, 0.01, 0, 0.01, 0" ), to = c("weight:low GDP", "weight:equal", "weight:high GDP"))
df2$weightname = factor(df2$weightname, levels=c("weight:low GDP", "weight:equal", "weight:high GDP"), labels=c("weight:low GDP", "weight:equal", "weight:high GDP")) 


theme_set(theme_bw())
#ggplot(data_m,aes(x=yearcat, value, group=interaction(year,Category)))+
ggplot(data_m %>% left_join(data.frame(weights=names(weight.labs), weights_label=weight.labs)),aes(x=yearcat, value, group=interaction(year,Category)))+
  theme_bw() + 
  geom_rect(xmin = 7.5, xmax = 14.5, ymin = -0.5, ymax = 1.5,
            fill = 'snow2', alpha = 0.05) +
  geom_jitter(aes(color=Category, group=interaction(year,Category)),width = 0.1, size=0.5) +
  labs(title = paste("AR6-database:", "welfare metric by rho and weight"),
       y = "", x = "")  + 
  scale_x_discrete(breaks = c("2030 C4","2060 C4","2100 C4"), labels=c("2030","2060","2100"))+
  facet_grid( rho ~ weightname, scales = "free_y", labeller=labeller(rho = rho.labs, weights = weight.labs))+
  scale_color_brewer(palette="RdYlBu", direction = -1)+ 
  #stat_summary(aes(fill = "Average"), fun = "mean", geom = "point", alpha = 0.15)+
  geom_point(data=df2, aes(x = yearcat, y = Welfare, fill = "Category mean"), shape=4)+
  labs(fill = "")
ggsave("figures/AR6_database - welfares.pdf", width=8, height = 6)

##########################################################################################
###############################correlation matrix between indicators #####################
##########################################################################################
corr_indicators <- indicators %>% filter(b_welfare>0) %>% filter(Category!='C8')

corr_temp <- corr_indicators %>% filter(variable=='Temperature')
corr_temp <- select(corr_temp, c(model, scenario,variable, year, indicator))
corr_nottemp <- corr_indicators %>% filter(variable!='Temperature')
corr_nottemp <- select(corr_nottemp, c(model, scenario,variable, year, indicator))


corr_data<-  corr_temp %>% left_join(corr_nottemp %>% pivot_wider(names_from = variable, values_from = "indicator") ) 
corr_data <- corr_data %>% rename(Temperature=indicator) %>% select(-c(scenario, model, variable, Consumption, `Food Demand`, `Emissions|CO2`, `Land Cover`, Population))

corr_data_2030=corr_data %>% filter( year==2030) %>% select(-year)
corr_data_2060=corr_data %>% filter( year==2060) %>% select(-year)
corr_data_2100=corr_data %>% filter( year==2100) %>% select(-year)


library(GGally)
library(gridExtra)
library(cowplot)

p1<-ggpairs(corr_data_2060, columns = 1:7, title="Year 2060")+ theme_grey(base_size = 8)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p2<-ggpairs(corr_data_2100, columns = 1:7, title="Year 2100")+ theme_grey(base_size = 8)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

pdf(file ="figures/CorrelationMatrix.pdf",width=12,height=6) 
plot_grid(
  ggmatrix_gtable(p1),
  ggmatrix_gtable(p2),
  nrow = 1, ncol=2)
dev.off()

#############################################################################################
##### compute equivalent increase in gdp for a 0.048 increase in welfare in 2030 #############
#############################################################################################

#From paper: When comparing the slope as of how the welfare index on average changes with global mean 
#temperature, we get the following values implying a change of between -0.01 and -0.05 
#of welfare for each degree of additional warming across scenarios, with a preferred value 
#of -0.048 for equal weights and a unity elasticity. 

#limiting to year 2100 and equal weights with rho=1:
selected_year=2100

welfaresyear=welfares %>% filter(year==selected_year) %>% filter(rho==1) %>% filter(selected_weights=="0.01, 0, 0, 0.005, 0.005, 0.01, 0.01, 0, 0.01, 0, 0.01, 0")
indicators_year <- indicators %>% filter(b_welfare>0) %>% filter(Category!='C8') %>% filter(year==selected_year)
variables_year<- indicators_year %>% filter(variable=='GDP')

#drop unnecessary columns

welfaresyear <- select(welfaresyear, c(model, scenario, value))
variables_year <- select(variables_year, c(model, scenario, value_pc, min, max))

#extract minimum of GDP value, delete min column and merge two dfs
min_GDP=unique(variables_year$min)
max_GDP=unique(variables_year$max)
variables_year <- select(variables_year, c(model, scenario, value_pc))

welfare_data<-  welfaresyear %>% left_join(variables_year, by=c('model', 'scenario') ) 

#calculate as r_rho1 the change in GDP per capita (as a share) that is equivalent to a change in welfare
#equal to diff_welfare
diff_welfare=-0.048
welfare_data= welfare_data %>% 
  mutate(r_rho1=exp( ( (1+diff_welfare/value)^(1/(1/6))   -1)*(log(value_pc)-log(min_GDP))   )-1)
welfare_data$GDP_reduced=welfare_data$value_pc*(1+welfare_data$r_rho1)

#get average:
print('average share in GDP change equivalent to welfare change for rho=1, equal weights:')
print(mean(welfare_data$r_rho1))
print('average GDP per capita before reduction:')
print(mean(welfare_data$value_pc))
print('average GDP per capita after reduction:')
print(mean(welfare_data$GDP_reduced))

#calculate change in GDP from regression line:

model=lm(welfare_data$value ~ welfare_data$value_pc)
r=1-(mean(welfare_data$value_pc-diff_welfare/model$coefficients[2]))/mean(welfare_data$value_pc)
print(r)

