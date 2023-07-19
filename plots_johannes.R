#Johannes plots

library("RColorBrewer")
theme_set(theme_bw())


combined_data <- welfares %>% select(-IMP_marker) %>% filter(!is.na(selected_weights)) %>% left_join(ar6_datadf %>% select(-unit) %>% pivot_wider(names_from = variable, values_from = "value") %>% mutate(gdppc=`GDP|PPP`/Population*1000)) %>% filter(Category!="C8")

combined_data$weightname <- plyr::mapvalues(combined_data$`weight_GDP|PPP`, from = c(0.01, 1, 100), to = c("weight:low GDP", "weight:equal", "weight:high GDP"))
combined_data$weightname = factor(combined_data$weightname, levels=c("weight:low GDP", "weight:equal", "weight:high GDP"), labels=c("weight:low GDP", "weight:equal", "weight:high GDP")) 

ggplot(combined_data %>% filter(year %in% c(2050, 2100) & Category!="failed-vetting")) + geom_point(aes(gdppc, value, color=Category)) + facet_wrap(year ~ .) + labs(x="GDP per capita [kUSD-$(2015)/cap]", y="Welfare index") + scale_color_brewer(palette="BrBG")#+ scale_fill_brewer(palette="BrBG")
ggsave("figures/Index GDP Scatter.png", width=8, height = 5)

ggplot(combined_data %>% filter(year %in% c(2100) & Category!="failed-vetting")) + geom_point(aes(gdppc, value, color=Category)) + facet_grid(paste0("rho:",rho) ~ weightname) + labs(x="GDP per capita [kUSD-$(2015)/cap]", y="Welfare index") + ylim(0,1) + scale_color_brewer(palette="BrBG")+ scale_fill_brewer(palette="BrBG")

ggsave("figures/Index GDP Scatter for rho 2100.png", width=8, height = 5)



#add regression line TEMP
ggplot(combined_data %>% filter(year %in% c(2100) & Category!="failed-vetting")) + geom_point(aes(`AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile`, value, color=Category)) + facet_grid(paste0("rho:",rho) ~ weightname) + labs(x="Temperature increase by 2100", y="Welfare index", color="Category") + theme(legend.position="bottom") + geom_smooth(aes(`AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile`, value), color="black", method="lm")  + ylim(0,1)  + scale_color_brewer(palette="BrBG")  + theme(legend.position = "right")
ggsave("figures/Temp Welfare Index.png", width=8, height = 5)


combined_data <- combined_data %>% mutate(gdppc=`GDP|PPP`/Population)

#coefficients
coefficients_temp_welfare <- combined_data %>% filter(year %in% c(2100) & Category!="failed-vetting") %>% nest_by(rho, weightname) %>% mutate(reg = list(lm(value ~ `AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile`, data = data))) %>% summarize(broom::tidy(reg))
openxlsx::write.xlsx(coefficients_temp_welfare, file="reg_temp.xlsx")


#add regression line GDP
ggplot(combined_data %>% filter(year %in% c(2100) & Category!="failed-vetting")) + geom_point(aes(gdppc, value, color=Category)) + facet_grid(paste0("rho:",rho) ~ weightname) + labs(x="GDP per capita in 2100 [kUSD-$(PPP)/cap]", y="Welfare index", color="Category") + theme(legend.position="bottom") + geom_smooth(aes(gdppc, value), color="black", method="lm")  + ylim(0,1)  + scale_color_brewer(palette="BrBG") + theme(legend.position = "right")
ggsave("figures/GDP Welfare Index.png", width=8, height = 5)

#coefficients
coefficients_temp_welfare <- combined_data %>% filter(year %in% c(2100) & Category!="failed-vetting") %>% nest_by(rho, weightname) %>% mutate(reg = list(lm(value ~ gdppc, data = data))) %>% summarize(broom::tidy(reg))
openxlsx::write.xlsx(coefficients_temp_welfare, file="reg_gdp.xlsx")





#now all weights
combined_data_allweights <- welfares %>% select(-IMP_marker) %>% left_join(ar6_datadf %>% select(-unit) %>% pivot_wider(names_from = variable, values_from = "value") %>% mutate(gdppc=`GDP|PPP`/Population*1000)) %>% filter(Category!="C8")

#get scenario dataframe of specific normalized weights
combined_data_allweights_onlyweights <- combined_data_allweights %>% mutate(sum_of_weights = across(starts_with("weight_")) %>% rowSums) %>% filter(year %in% c(2100) & Category!="failed-vetting") %>% group_by(rho, weights) %>% dplyr::summarise(weight_report = mean(`weight_Food Energy Supply`/sum_of_weights))

#GDPPC
coefficients_gdppc_welfare_allweights <- combined_data_allweights %>% filter(year %in% c(2100) & Category!="failed-vetting") %>% nest_by(rho, weights) %>% mutate(reg = list(lm(value ~ gdppc, data = data))) %>% summarize(broom::tidy(reg))
ggplot(coefficients_gdppc_welfare_allweights %>% filter(term=="gdppc") %>% left_join(combined_data_allweights_onlyweights), aes(x = as.factor(rho), y = estimate*1e4, color=weight_report)) + geom_violin(draw_quantiles = c(0.5), alpha=0.6) +
  labs(x="Rho", y="Slope of Welfare - GDP per capita relationship", title = "Slope (welfare points per 10,000 USD)", color = "Food Supply weight") + geom_jitter(position = position_jitter(seed = 1, width = 0.1), alpha = 0.4) + geom_hline(yintercept = 0) + theme(legend.position = "bottom")
ggsave("figures/violin_gdp.png")

#Temperature
coefficients_temp_welfare_allweights <- combined_data_allweights %>% filter(year %in% c(2100) & Category!="failed-vetting") %>% nest_by(rho, weights) %>% mutate(reg = list(lm(value ~ `AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile`, data = data))) %>% summarize(broom::tidy(reg))
ggplot(coefficients_temp_welfare_allweights %>% filter(term=="`AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile`") %>% left_join(combined_data_allweights_onlyweights), aes(x = as.factor(rho), y = estimate, color=weight_report)) + geom_violin(draw_quantiles = c(0.5), alpha=0.6) +
  labs(x="Rho", y="Slope of Welfare - Temperature relationship", title = "Slope (welfare points per 1 degree C)", color = "Food Supply weight") + geom_jitter(position = position_jitter(seed = 1, width = 0.1), alpha = 0.4) + geom_hline(yintercept = 0) + theme(legend.position = "bottom")
ggsave("figures/violin_temp.png")
















#Radar chart
library(ggplot2)
library(ggradar)
suppressPackageStartupMessages(library(dplyr))
library(scales)




combined_data_based_on_indicators <- welfares %>% left_join(indicators %>% select(-unit, -value, value_pc, -min, -max, -bad, -log, -identifier, -value_pc) %>% pivot_wider(names_from = variable, values_from = "indicator"))



ggradar(combined_data_based_on_indicators %>% select(-`Emissions|CO2`, -Consumption, -Population, -`Land Cover`, -`Food Demand`) %>% mutate(group=paste(model, scenario)) %>% filter(year==2100) %>% select(22, 7, 15:21) %>% rename(Welfare=value) %>% tail(50000),  plot.legend = F, group.point.size = 1, group.line.width = .1)
ggsave("figures/Radar Plot.png", width=8, height = 6)

#ggplot(combined_data_based_on_indicators %>% filter(year %in% c(2100) & Category!="failed-vetting")) + geom_point(aes(`GDP|PPP`, value, color=Category)) + facet_wrap(year ~ .) + labs(x="GDP per capita Index", y="Welfare index")
#ggsave("figures/Index GDP Scatter indicators.png", width=8, height = 5)


#ggplot(combined_data_based_on_indicators %>% filter(year %in% c(2100) & Category!="failed-vetting")) + geom_point(aes(`GDP|PPP`, `AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile`, color=value)) + facet_wrap(year ~ .) + labs(x="GDP per capita [kUSD-$(2015)/cap]", y="GMT Increase") + scale_colour_gradient(low = "red", high = "blue")
#ggsave("figures/Temp GDP Scatter indicators.png", width=8, height = 5)


#215 scenarios including 1 C8 scenario






