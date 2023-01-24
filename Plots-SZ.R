### Preparing data: select baseline welfare metric, remove Categories "failed-vetting" and C8, select only decades 
### Produce simpler names for the underlying variables

welfare_base <- subset(welfares,rho==1&weights=="1,0,0,0.5,0.5,1,1,0,1,0,1,0")
welfare_base <- filter(welfare_base, year != "", value != "", Category != "failed-vetting", Category != "C8") %>%
  dplyr:: filter(!is.na(value)) %>% 
  filter(year %in% seq(from = 2010, to = 2100, by = 10)) 
welfare_base$value<-as.numeric(welfare_base$value) 

indicators_2 <- filter(indicators, year != "", indicator != "", Category != "failed-vetting", Category != "C8") %>%
  dplyr:: filter(!is.na(indicator)) %>% 
  filter(year %in% seq(from = 2010, to = 2100, by = 10)) 
indicators_2$indicator<-as.numeric(indicators_2$indicator) 

indicators_2$variable2 <- indicators_2$variable
indicators_2 <- indicators_2 %>%
  mutate(variable2=recode(variable2, "AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile"="Temperature","Comsumption"="Comsumption","Emissions|CO2"="CO2_Emissions","Emissions|NOx"="NOx_Emissions","Emissions|Sulfur"="Sulfur_Emissions","Final Energy|Electricity"="Electricity","Food Demand"="Food_Demand", "Food Energy Supply"="Food","GDP|PPP"="Per_capita_GDP","Land Cover"="Land_Cover","Land Cover|Forest"="Forest_Cover","Population"="Population"))


### Scatter Plots

library(gridExtra)

theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_blank())

PWelf2060 <- ggplot(subset(welfare_base,year==2060), aes(x=Category, y=value, color=Category)) +
  geom_point(size=1) + ggtitle("Welfare",subtitle = "2060") + coord_cartesian(ylim=c(0,1)) + guides(col = guide_legend(nrow = 4))

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
           geom_point(size=1) + theme(legend.position="none") + ggtitle(" ",subtitle = paste0(i)) + coord_cartesian(ylim=c(0,1)))
}

# grid.arrange(PWelf2030, PWelf2060, PWelf2100, legend, ncol=4, widths=c(2, 2, 2, 2))

varset <- unique(indicators_2$variable2)

for(j in varset) 
{                    
  assign(paste0("P",j,"2060"), ggplot(subset(indicators_2,year==2060&variable2==j), aes(x=Category, y=indicator, color=Category)) +
           geom_point(size=1) + theme(legend.position="none") + ggtitle(paste0(j),subtitle = paste0(i)) + coord_cartesian(ylim=c(0,1)))
}


for(j in varset) 
{                    
    for (i in c(2030,2100))
      {
      assign(paste0("P",j,i), ggplot(subset(indicators_2,year==i&variable2==j), aes(x=Category, y=indicator, color=Category)) +
           geom_point(size=1) + theme(legend.position="none") + ggtitle("",subtitle = paste0(i)) + coord_cartesian(ylim=c(0,1)))
    }
}  

png(file ="figures/ScatterPlot-Cat.png",width=15,height=7,units="in",res=1200,pointsize=4) 
p <- grid.arrange(PTemperature2030, PTemperature2060, PTemperature2100, PNOx_Emissions2030, PNOx_Emissions2060, PNOx_Emissions2100, legend, 
             PSulfur_Emissions2030, PSulfur_Emissions2060, PSulfur_Emissions2100, PElectricity2030, PElectricity2060, PElectricity2100, 
             PPer_capita_GDP2030, PPer_capita_GDP2060, PPer_capita_GDP2100, PForest_Cover2030, PForest_Cover2060, PForest_Cover2100, 
             PFood2030, PFood2060, PFood2100, PWelf2030, PWelf2060, PWelf2100, 
             ncol=9, nrow=4, widths=c(2, 2, 2, 0.3, 2, 2, 2, 0.3, 2),layout_matrix=rbind(c(1,2,3,NA,4,5,6,NA,7),
                                                                                         c(8,9,10,NA,11,12,13,NA,NA),
                                                                                         c(14,15,16,NA,17,18,19,NA,NA),
                                                                                         c(20,21,22,NA,23,24,25,NA,NA)))
print(p)
dev.off()


### Box Plots

PWelf2060 <- ggplot(subset(welfare_base,year==2060), aes(x=Category, y=value, color=Category)) +
  geom_boxplot() + theme(legend.position="none") + ggtitle("Welfare",subtitle = "2060") + coord_cartesian(ylim=c(0,1))


PWelf2060 <- PWelf2060+ theme(legend.position="none")

for(i in c(2030,2100)) 
{                    
  assign(paste0("PWelf", i), ggplot(subset(welfare_base,year==i), aes(x=Category, y=value, color=Category)) +
           geom_boxplot() + theme(legend.position="none") + ggtitle(" ",subtitle = paste0(i)) + coord_cartesian(ylim=c(0,1)))
}

for(j in varset) 
{                    
  assign(paste0("P",j,"2060"), ggplot(subset(indicators_2,year==2060&variable2==j), aes(x=Category, y=indicator, color=Category)) +
           geom_boxplot() + theme(legend.position="none") + ggtitle(paste0(j),subtitle = paste0(i)) + coord_cartesian(ylim=c(0,1)))
}


for(j in varset) 
{                    
  for (i in c(2030,2100))
  {
    assign(paste0("P",j,i), ggplot(subset(indicators_2,year==i&variable2==j), aes(x=Category, y=indicator, color=Category)) +
             geom_boxplot() + theme(legend.position="none") + ggtitle("",subtitle = paste0(i)) + coord_cartesian(ylim=c(0,1)))
  }
}  

png(file ="figures/BoxPlot-Cat.png",width=15,height=7,units="in",res=1200,pointsize=4) 
p <- grid.arrange(PTemperature2030, PTemperature2060, PTemperature2100, PNOx_Emissions2030, PNOx_Emissions2060, PNOx_Emissions2100, legend, 
                  PSulfur_Emissions2030, PSulfur_Emissions2060, PSulfur_Emissions2100, PElectricity2030, PElectricity2060, PElectricity2100, 
                  PPer_capita_GDP2030, PPer_capita_GDP2060, PPer_capita_GDP2100, PForest_Cover2030, PForest_Cover2060, PForest_Cover2100, 
                  PFood2030, PFood2060, PFood2100, PWelf2030, PWelf2060, PWelf2100, 
                  ncol=9, nrow=4, widths=c(2, 2, 2, 0.3, 2, 2, 2, 0.3, 2),layout_matrix=rbind(c(1,2,3,NA,4,5,6,NA,7),
                                                                                              c(8,9,10,NA,11,12,13,NA,NA),
                                                                                              c(14,15,16,NA,17,18,19,NA,NA),
                                                                                              c(20,21,22,NA,23,24,25,NA,NA)))
print(p)
dev.off()

