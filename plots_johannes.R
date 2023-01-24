#Johannes plots


combined_data <- welfares %>% left_join(ar6_datadf %>% select(-unit) %>% pivot_wider(names_from = variable, values_from = "value"))

ggplot(combined_data %>% filter(year %in% c(2050, 2100) & Category!="failed-vetting")) + geom_point(aes(`GDP|PPP`/Population, value, color=Category)) + facet_wrap(year ~ .) + labs(x="GDP per capita [kUSD-$(2015)/cap]", y="Welfare index")
ggsave("figures/Index GDP Scatter.png", width=8, height = 5)


ggplot(combined_data %>% filter(year %in% c(2100) & Category!="failed-vetting")) + geom_point(aes(`GDP|PPP`/Population, `AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile`, color=value)) + facet_wrap(year ~ .) + labs(x="GDP per capita [kUSD-$(2015)/cap]", y="GMT Increase") + scale_colour_gradient(low = "red", high = "blue")
ggsave("figures/Temp GDP Scatter.png", width=8, height = 5)



#Radar chart
library(ggplot2)
library(ggradar)
suppressPackageStartupMessages(library(dplyr))
library(scales)




combined_data_based_on_indicators <- welfares %>% left_join(indicators %>% select(-unit, -value, value_pc, -min, -max, -bad, -log, -identifier, -value_pc) %>% pivot_wider(names_from = variable, values_from = "indicator"))



ggradar(combined_data_based_on_indicators %>% mutate(group=paste(model, scenario)) %>% filter(year==2100) %>% select(-`Food Demand`) %>% select(26, 7, 15:25) %>% rename(Welfare=value) %>% tail(50000),  plot.legend = F, group.point.size = 1, group.line.width = 1)
ggsave("figures/Radar Plot.png", width=8, height = 6)

ggplot(combined_data_based_on_indicators %>% filter(year %in% c(2100) & Category!="failed-vetting")) + geom_point(aes(`GDP|PPP`, value, color=Category)) + facet_wrap(year ~ .) + labs(x="GDP per capita Index", y="Welfare index")
ggsave("figures/Index GDP Scatter indicators.png", width=8, height = 5)


ggplot(combined_data_based_on_indicators %>% filter(year %in% c(2100) & Category!="failed-vetting")) + geom_point(aes(`GDP|PPP`, `AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile`, color=value)) + facet_wrap(year ~ .) + labs(x="GDP per capita [kUSD-$(2015)/cap]", y="GMT Increase") + scale_colour_gradient(low = "red", high = "blue")
ggsave("figures/Temp GDP Scatter indicators.png", width=8, height = 5)


#215 scenarios including 1 C8 scenario







#TO DO FOR THURSDAY:


1) send min max based on R5 to Rike
2) scatters with parameter values/rho
3) update scatter.smooth(
  
  
  
  
)
