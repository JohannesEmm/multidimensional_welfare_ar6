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
  load("ar6_data.Rdata")
  
}


library("RColorBrewer")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(writexl)
library("PerformanceAnalytics")
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
#png(file = paste("figures/","AR6_database- variables",".png",sep=""), width = 12000, height = 12000, units = "px") 

ggplot(data_m) +
  geom_line(aes(x=year, y=value, color=Category, group=interaction(model,scenario)), size=0.1) + 
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
#dplyr::filter(Category!="C8") #removing C8 because it is only one scenario in here

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




################################## characterize scenarios                ##################
###########################################################################################
###########################################################################################


df_test=filter(welfare_plot, year=="2060")


df2 <- df_test %>% group_by(Category) %>% 
  summarise(N=sum(value)/mean(value),
            Models=paste(unique(model), collapse=', ' ),
            Welfare=mean(value),
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
  geom_point(size=1)+scale_color_brewer(palette="RdYlBu", direction = -1) + 
  ggtitle("Welfare",subtitle = "2060") + coord_cartesian(ylim=c(0,1)) + guides(col = guide_legend(nrow = 4))

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
           scale_color_brewer(palette="RdYlBu", direction = -1) + theme(legend.position="none") + ggtitle(" ",subtitle = paste0(i)) + coord_cartesian(ylim=c(0,1)))
}

# grid.arrange(PWelf2030, PWelf2060, PWelf2100, legend, ncol=4, widths=c(2, 2, 2, 2))

varset <- unique(indicators_2$variable2)
#varset2<- unique(indicators_2$variable3)
i=2060
for(j in varset) 
{                    
  assign(paste0("P",gsub(" ", "", j),"2060"), ggplot(subset(indicators_2,year==2060&variable2==j), aes(x=Category, y=indicator, color=Category)) +
           geom_point(size=1)+
           scale_color_brewer(palette="RdYlBu", direction = -1) + theme(legend.position="none") + ggtitle(paste0(j),subtitle = paste0(i)) + coord_cartesian(ylim=c(0,1)))
}


for(j in varset) 
{                    
  for (i in c(2030,2100))
  {
    assign(paste0("P",gsub(" ", "", j),i), ggplot(subset(indicators_2,year==i&variable2==j), aes(x=Category, y=indicator, color=Category)) +
             geom_point(size=1)+
             scale_color_brewer(palette="RdYlBu", direction = -1) + theme(legend.position="none") + ggtitle("",subtitle = paste0(i)) + coord_cartesian(ylim=c(0,1)))
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

#####################################reduced plots for NAVIGATE report##########################


library(gridExtra)

theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), axis.title.x = element_blank(), axis.title.y = element_blank())

pdf(file ="figures/NAVIGATE1.pdf",width=6,height=8) 
PWelf2100 <- ggplot(subset(welfare_base,year==2100), aes(Category, value)) +
  geom_point(size=1, show.legend = FALSE)+ggtitle("", subtitle="Welfare") + coord_cartesian(ylim=c(0,1)) +
  theme(text = element_text(size=15))
print(PWelf2100)
dev.off()



# grid.arrange(PWelf2030, PWelf2060, PWelf2100, legend, ncol=4, widths=c(2, 2, 2, 2))

varset <- unique(indicators_2$variable2)
#varset2<- unique(indicators_2$variable3)

for(j in varset) 
{                    
  i=2100
  {
    assign(paste0("P",gsub(" ", "", j),i), ggplot(subset(indicators_2,year==i&variable2==j), aes(x=Category, y=indicator)) +
             geom_point(size=1)+
             theme(text = element_text(size=15))+
             theme(legend.position="none") + ggtitle("", subtitle= paste0(j)) + coord_cartesian(ylim=c(0,1)))
  }
}  

pdf(file ="figures/NAVIGATE2.pdf",width=12.5,height=8) 
p <- grid.arrange(PTemperature2100, PGDP2100, 
                  PNOxEmissions2100, PSulfurEmissions2100, PForestCover2100, 
                  PFoodSupply2100,PElectricity2100, PWelf2100, 
                  ncol=4, nrow=2)
print(p)
dev.off()


#########################################################################################
##################################### diagnostics on welfare  ###########################
#########################################################################################

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

df2 <- data_m %>% group_by(interaction(Category, year, rho, weights)) %>% 
  summarise(
    Welfare=mean(value),
    .groups = 'drop') %>%
  as.data.frame()
require("writexl")
write_xlsx(df2, 'AR6-summary.xlsx')


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


p=(ggplot(data_m %>% left_join(data.frame(weights=names(weight.labs), weights_label=weight.labs)),aes(x=yearcat, value, group=interaction(year,Category)))+
     theme_bw() + 
     geom_rect(xmin = 7.5, xmax = 14.5, ymin = -0.5, ymax = 1.5,
               fill = 'snow2', alpha = 0.05) +
     geom_jitter(aes(color=Category, group=interaction(year,Category)),width = 0.1, cex=2) +
     labs(title = paste("AR6-database:", "welfare metric by rho and weight"),
          y = "", x = "")  + 
     scale_x_discrete(breaks = c("2030 C4","2060 C4","2100 C4"), labels=c("2030","2060","2100"))+
     facet_grid( rho ~ factor(weights_label, levels=rev(weight.labs)), scales = "free_y", labeller=labeller(rho = rho.labs))+
     scale_color_brewer(palette="RdYlBu", direction = -1)+#geom_point(data=data_m, shape=7, aes(x=yearcat, y=Mean, group = Category, colour=Category),size=2)+ # here you can see that the distribution does not really deliver a meaningful mean
     scale_fill_brewer(palette="BrBG")+ theme(text = element_text(size = 28))  )
print(p)
dev.off()

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

#pdf(file ="figures/CorrelationMatrix2030.pdf",width=5.63,height=3.91) 
#p<-par(mar = c(5.1, 4.1, 15, 2.1))#default (5.1, 4.1, 4.1, 2.1).
#p<-chart.Correlation(corr_data_2030, histogram = TRUE, pch = 19)
#p<-mtext("Year 2030", side=3, line=14, cex=1)
#print(p)
#dev.off()

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


#value of GDP per capita in 2020:

selected_year=2020

welfaresyear=welfares %>% filter(year==selected_year) %>% filter(rho==1) %>% filter(selected_weights=="0.01, 0, 0, 0.005, 0.005, 0.01, 0.01, 0, 0.01, 0, 0.01, 0")
indicators_year <- indicators %>% filter(b_welfare>0) %>% filter(Category!='C8') %>% filter(year==selected_year)
variables_year<- indicators_year %>% filter(variable=='GDP')

#drop unnecessary columns

welfaresyear <- select(welfaresyear, c(model, scenario, value))
variables_year <- select(variables_year, c(model, scenario, value_pc))
welfare_data<-  welfaresyear %>% left_join(variables_year, by=c('model', 'scenario') ) 
print('average GDP per capita in 2020:')
print(mean(welfare_data$value_pc))


# 
# ### now for all weights and rhos:
# welfaresyear=welfares %>% filter(year==selected_year) 
# welfaresyear=welfaresyear %>% 
#   mutate(
#     aggregate_weight= select(., starts_with("weight_")) %>% rowSums()
#   )
# 
# welfare_data <- select(welfaresyear, c(model, scenario, rho, `weight_GDP|PPP`, aggregate_weight,value))
# welfare_data<-  welfare_data %>% left_join(variables_year, by=c('model', 'scenario') ) 
# 
# #calculate as r the change in GDP per capita (as a share) that is equivalent to a change in welfare
# #equal to diff_welfare
# 
# welfare_data= welfare_data %>% 
#   mutate(r=(rho==1)*(exp( ( (1+diff_welfare/value)^(1/(`weight_GDP|PPP`/aggregate_weight))   -1)*(log(value_pc)-log(min_GDP))   )-1 )
#          + (1-(rho==1))*(exp( (1/(`weight_GDP|PPP`/aggregate_weight)*( (value+diff_welfare)^(1-rho*(rho!=1)) - value^(1-rho*(rho!=1)) 
#                                                               + `weight_GDP|PPP`/aggregate_weight*((log(value_pc)-log(min_GDP))/(log(max_GDP)-log(min_GDP)))^(1-rho*(rho!=1))))^(1/(1-rho*(rho!=1)))*(log(max_GDP)-log(min_GDP))+log(min_GDP) -log(value_pc) )-1))
# 
# #get average:
# print('average share in GDP change equivalent to welfare change across all welfare parameters:')
# print(mean(welfare_data$r))
# print('average GDP per capita:')
# print(mean(welfare_data$value_pc))
# 
# 







