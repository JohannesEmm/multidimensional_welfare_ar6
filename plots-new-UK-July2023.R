# this plots the welfare data, run 'main.R' first
# saves in subfolder "figures"

rm(list = ls())

### if welfares already exist, just load Rdata file
### if welfares exist, indicator.Rdata and weights.Rdata is assumed to exist as well, so this will also load

if(!file.exists("welfares.Rdata"))
{
print('Run main.R file first')
  
}else{
  load("indicators.Rdata")
  load("welfares.Rdata")
  load("weights.Rdata")
}


library("RColorBrewer")
library(tidyverse)
library(dplyr)

if(!dir.exists("figures")){dir.create("figures")}


  
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
  png(file = paste("figures/","AR6_database- variables",".png",sep=""), width = 12000, height = 12000, units = "px") 
  
  p=(ggplot(data_m) +
       geom_line(aes(x=year, y=value, color=Category, group=interaction(model,scenario)), alpha = 0.7) + 
       labs(title = paste("AR6-database:", "variables for welfare metric"),
            y = "", x = "Time") + 
       scale_x_continuous(breaks = seq(from = 2000, to = 2099, by = 50))+
       facet_wrap( ~ variable, scales = "free", ncol=4)+
       scale_color_brewer(palette="BrBG")+ scale_fill_brewer(palette="BrBG")+ theme(text = element_text(size = 200)))
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
  welfares_plot=welfares
  # pick which welfare parameters to choose:
  welfare_plot= welfares_plot %>%
    filter(rho==1 & weights=="1, 0, 0, 0.5, 0.5, 1, 1, 0, 1, 0, 1, 0") #equal weights and some substitutability
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
  #df_test=filter(indicators, Category=="C8" & variable=="GDP|PPP" & b_welfare >0 & year=="2030")
  # in  2030,2060,2100:
  #C1 has 13 scenarios
  #C2 has 25 scenarios
  #C3 has 60 scenarios
  #C4 has 31 scenarios
  #C5 has 43 scenarios
  #C6 has 18 scenarios
  #C7 has 24 scenarios
  #C8 has 1 scenario
  #failed vetting: 56 scenarios
  
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
     theme_bw() + 
     geom_rect(xmin = 8.5, xmax = 16.5, ymin = -0.5, ymax = 1.5,
               fill = 'snow2', alpha = 0.05) +
  geom_jitter(aes(color=Category, group=interaction(year,Category)),width = 0.1, cex=2) + 
  labs(title = paste("AR6-database: ", "Indicators and welfare"),
       y = "", x = "") + 
    scale_x_discrete(breaks = c("2030 C4","2060 C4","2100 C4"), labels=c("2030","2060","2100"),expand=c(0.05, 0))+
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
     geom_line(aes(y=Mean, color=Category), linewidth=1) + 
     geom_ribbon(aes(y = Mean, ymin = Mean - SD, ymax = Mean + SD, fill = Category), alpha = .1)+
     labs(title = paste("AR6-database:", "mean indicators and welfare"),
          y = "", x = "") + 
     scale_x_continuous(breaks = seq(from = 2020, to = 2099, by = 30))+
     facet_wrap( ~ variable , scales = "free", ncol=4)+
     scale_color_brewer(palette="BrBG")+
     scale_fill_brewer(palette="BrBG")+ theme(text = element_text(size = 28)))
print(p)
dev.off()




###########################################################################################
################################## characterize scenarios                ##################
###########################################################################################


df_test=filter(indicators_plot, variable=="GDP|PPP" & year=="2030")


df2 <- df_test %>% group_by(Category) %>% 
  summarise(N=sum(value)/mean(value),
            Models=paste(unique(model), collapse=', ' ),
            .groups = 'drop') %>%
  as.data.frame()
require("writexl")
write_xlsx(df2, 'AR6-summary.xlsx')


###########################################################################################
################################## prepare plots for 1 welfare and indicators##############
###########################################################################################

# ammend the welfare to data_plot
welfares_plot=welfares
#pick which welfare parameters to choose:
welfare_plot= welfares_plot %>%
  filter(rho==1 & weights=="1, 0, 0, 0.5, 0.5, 1, 1, 0, 1, 0, 1, 0") #equal weights and some substitutability

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
#indicators_2$variable3 <- indicators_2$variable
#indicators_2 <- indicators_2 %>%
#  mutate(variable2=recode(variable2, "AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile"="Temperature","Comsumption"="Comsumption","Emissions|CO2"="CO2_Emissions","Emissions|NOx"="NOx_Emissions","Emissions|Sulfur"="Sulfur_Emissions","Final Energy|Electricity"="Electricity","Food Demand"="Food_Demand", "Food Energy Supply"="Food","GDP|PPP"="Per_capita_GDP","Land Cover"="Land_Cover","Land Cover|Forest"="Forest_Cover","Population"="Population"))
indicators_2 <- indicators_2 %>%
  mutate(variable2=recode(variable2, "AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile"="Temperature","Comsumption"="Comsumption","Emissions|CO2"="CO2 Emissions","Emissions|NOx"="NOx Emissions","Emissions|Sulfur"="Sulfur Emissions","Final Energy|Electricity"="Electricity","Food Demand"="Food Demand", "Food Energy Supply"="Food Supply","GDP|PPP"="GDP","Land Cover"="Land Cover","Land Cover|Forest"="Forest Cover","Population"="Population"))


### Scatter Plots

library(gridExtra)

theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank())

PWelf2060 <- ggplot(subset(welfare_base,year==2060), aes(x=Category, y=value, color=Category)) +
  geom_point(size=1)+
  scale_color_brewer(palette="BrBG") + ggtitle("Welfare",subtitle = "2060") + coord_cartesian(ylim=c(0,1)) + guides(col = guide_legend(nrow = 4))

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
           scale_color_brewer(palette="BrBG") + theme(legend.position="none") + ggtitle(" ",subtitle = paste0(i)) + coord_cartesian(ylim=c(0,1)))
}

# grid.arrange(PWelf2030, PWelf2060, PWelf2100, legend, ncol=4, widths=c(2, 2, 2, 2))

varset <- unique(indicators_2$variable2)
#varset2<- unique(indicators_2$variable3)
i=2060
for(j in varset) 
{                    
  assign(paste0("P",gsub(" ", "", j),"2060"), ggplot(subset(indicators_2,year==2060&variable2==j), aes(x=Category, y=indicator, color=Category)) +
           geom_point(size=1)+
           scale_color_brewer(palette="BrBG") + theme(legend.position="none") + ggtitle(paste0(j),subtitle = paste0(i)) + coord_cartesian(ylim=c(0,1)))
}


for(j in varset) 
{                    
  for (i in c(2030,2100))
  {
    assign(paste0("P",gsub(" ", "", j),i), ggplot(subset(indicators_2,year==i&variable2==j), aes(x=Category, y=indicator, color=Category)) +
             geom_point(size=1)+
             scale_color_brewer(palette="BrBG") + theme(legend.position="none") + ggtitle("",subtitle = paste0(i)) + coord_cartesian(ylim=c(0,1)))
  }
}  

png(file ="figures/ScatterPlot-Cat.png",width=12.5,height=8,units="in",res=1500,pointsize=4) 
p <- grid.arrange(PTemperature2030, PTemperature2060, PTemperature2100, PGDP2030, PGDP2060, PGDP2100, legend, 
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
names(weight.labs) <- c("1, 0, 0, 0.5, 0.5, 1, 100, 0, 1, 0, 1, 0", "1, 0, 0, 0.5, 0.5, 1, 1, 0, 1, 0, 1, 0", "1, 0, 0, 0.5, 0.5, 1, 0.01, 0, 1, 0, 1, 0")
rho.labs <- c("rho:0", "rho:1", "rho:5")
names(rho.labs) <- c("0", "1", "5")

welf_plot_red= welfares %>%
  filter(year %in% c(2030,2060,2100) & weights %in% c("1, 0, 0, 0.5, 0.5, 1, 100, 0, 1, 0, 1, 0", "1, 0, 0, 0.5, 0.5, 1, 1, 0, 1, 0, 1, 0", "1, 0, 0, 0.5, 0.5, 1, 0.01, 0, 1, 0, 1, 0"))
welf_plot_red$yearcat=as.factor(paste(welf_plot_red$year, welf_plot_red$Category))

data_m <- welf_plot_red%>%
  dplyr::filter(Category!="failed-vetting")

theme_set(theme_bw())
png(file = paste("figures/","AR6_database welfares",".png",sep=""), width = 1200, height = 1200, units = "px") 
p=(ggplot(data_m,aes(x=yearcat, value, group=interaction(year,Category)))+
     theme_bw() + 
     geom_rect(xmin = 8.5, xmax = 16.5, ymin = -0.5, ymax = 1.5,
               fill = 'snow2', alpha = 0.05) +
     geom_jitter(aes(color=Category, group=interaction(year,Category)),width = 0.1, cex=2) +
     labs(title = paste("AR6-database:", "welfare metric by rho and weight"),
          y = "", x = "")  + 
     scale_x_discrete(breaks = c("2030 C4","2060 C4","2100 C4"), labels=c("2030","2060","2100"))+
     facet_grid( rho ~ weights, scales = "free_y", labeller=labeller(rho = rho.labs, weights = weight.labs))+
     scale_color_brewer(palette="BrBG")+#geom_point(data=data_m, shape=7, aes(x=yearcat, y=Mean, group = Category, colour=Category),size=2)+ # here you can see that the distribution does not really deliver a meaningful mean
     scale_fill_brewer(palette="BrBG")+ theme(text = element_text(size = 28))  )
print(p)
dev.off()







