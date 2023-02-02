#Johannes plots

library("RColorBrewer")
theme_set(theme_bw())


combined_data <- welfares %>% left_join(ar6_datadf %>% select(-unit) %>% pivot_wider(names_from = variable, values_from = "value")) %>% filter(Category!="C8")

combined_data$weightname <- plyr::mapvalues(combined_data$weights, from = c("1,0,0,0.5,0.5,1,0.2,0,1,0,1,0", "1,0,0,0.5,0.5,1,1,0,1,0,1,0", "1,0,0,0.5,0.5,1,100,0,1,0,1,0"), to = c("weight:low GDP", "weight:equal", "weight:high GDP"))
combined_data$weightname = factor(combined_data$weightname, levels=c("weight:low GDP", "weight:equal", "weight:high GDP"), labels=c("weight:low GDP", "weight:equal", "weight:high GDP")) 

ggplot(combined_data %>% filter(year %in% c(2050, 2100) & Category!="failed-vetting")) + geom_point(aes(`GDP|PPP`/Population, value, color=Category)) + facet_wrap(year ~ .) + labs(x="GDP per capita [kUSD-$(2015)/cap]", y="Welfare index") + scale_color_brewer(palette="BrBG")#+ scale_fill_brewer(palette="BrBG")
ggsave("figures/Index GDP Scatter.png", width=8, height = 5)

ggplot(combined_data %>% filter(year %in% c(2100) & Category!="failed-vetting")) + geom_point(aes(`GDP|PPP`/Population, value, color=Category)) + facet_grid(paste0("rho:",rho) ~ weightname) + labs(x="GDP per capita [kUSD-$(2015)/cap]", y="Welfare index") + ylim(0,1) + scale_color_brewer(palette="BrBG")+ scale_fill_brewer(palette="BrBG")

ggsave("figures/Index GDP Scatter for rho 2100.png", width=8, height = 5)


ggplot(combined_data %>% filter(year %in% c(2100) & Category!="failed-vetting")) + geom_point(aes(`AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile`, value, color=Category)) + facet_grid(paste0("rho:",rho) ~ weightname) + labs(x="Temperature increase by 2100", y="Welfare index", color="Category") + theme(legend.position="bottom") + geom_smooth(aes(`AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile`, value), color="black", method="lm")  + ylim(0,1)  + scale_color_brewer(palette="BrBG")
ggsave("figures/Temp Welfare Index.png", width=8, height = 5)

#coefficients
coefficients_temp_welfare <- combined_data %>% filter(year %in% c(2100) & Category!="failed-vetting") %>% nest_by(rho, weightname) %>% mutate(reg = list(lm(value ~ `AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile`, data = data))) %>% summarize(broom::tidy(reg))
openxlsx::write.xlsx(coefficients_temp_welfare, file="reg.xlsx")


#Radar chart
library(ggplot2)
library(ggradar)
suppressPackageStartupMessages(library(dplyr))
library(scales)




combined_data_based_on_indicators <- welfares %>% left_join(indicators %>% select(-unit, -value, value_pc, -min, -max, -bad, -log, -identifier, -value_pc) %>% pivot_wider(names_from = variable, values_from = "indicator"))



ggradar(combined_data_based_on_indicators %>% mutate(group=paste(model, scenario)) %>% filter(year==2100) %>% select(-`Food Demand`) %>% select(26, 7, 15:25) %>% rename(Welfare=value) %>% tail(50000),  plot.legend = F, group.point.size = 1, group.line.width = 1)
ggsave("figures/Radar Plot.png", width=8, height = 6)

#ggplot(combined_data_based_on_indicators %>% filter(year %in% c(2100) & Category!="failed-vetting")) + geom_point(aes(`GDP|PPP`, value, color=Category)) + facet_wrap(year ~ .) + labs(x="GDP per capita Index", y="Welfare index")
#ggsave("figures/Index GDP Scatter indicators.png", width=8, height = 5)


#ggplot(combined_data_based_on_indicators %>% filter(year %in% c(2100) & Category!="failed-vetting")) + geom_point(aes(`GDP|PPP`, `AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile`, color=value)) + facet_wrap(year ~ .) + labs(x="GDP per capita [kUSD-$(2015)/cap]", y="GMT Increase") + scale_colour_gradient(low = "red", high = "blue")
#ggsave("figures/Temp GDP Scatter indicators.png", width=8, height = 5)


#215 scenarios including 1 C8 scenario









