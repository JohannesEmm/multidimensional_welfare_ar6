#Johannes plots
theme_set(theme_bw())


combined_data <- welfares %>% select(-IMP_marker) %>% filter(!is.na(selected_weights)) %>% left_join(ar6_datadf %>% select(-unit) %>% pivot_wider(names_from = variable, values_from = "value") %>% mutate(gdppc=`GDP|PPP`/Population*1000)) %>% filter(Category!="C8")

combined_data$weightname <- plyr::mapvalues(combined_data$selected_weights, from = c("1, 0, 0, 0.5, 0.5, 1, 0.01, 0, 1, 0, 1, 0" , "0.01, 0, 0, 0.005, 0.005, 0.01, 0.01, 0, 0.01, 0, 0.01, 0", "0.01, 0, 0, 0.005, 0.005, 0.01, 1, 0, 0.01, 0, 0.01, 0" ), to = c("weight:low GDP", "weight:equal", "weight:high GDP"))
combined_data$weightname = factor(combined_data$weightname, levels=c("weight:low GDP", "weight:equal", "weight:high GDP"), labels=c("weight:low GDP", "weight:equal", "weight:high GDP")) 

ggplot(combined_data %>% filter(year %in% c(2050, 2100) & Category!="failed-vetting")) + geom_point(aes(gdppc, value, color=Category)) + facet_wrap(year ~ .) + labs(x="GDP per capita [kUSD-$(2015)/cap]", y="Welfare index") + scale_color_brewer(palette="BrBG")#+ scale_fill_brewer(palette="BrBG")
ggsave("figures/Index GDP Scatter 2050 2100.png", width=8, height = 5)


#year_for_reg_line <- c(2100)
#year_for_reg_line <- c(2050)
year_for_reg_line <- c(2030)
#year_for_reg_line <- c(2060)
#year_for_reg_line <- seq(2025,2100,5)

ggplot(combined_data %>% filter(year %in% year_for_reg_line & Category!="failed-vetting")) + geom_point(aes(gdppc, value, color=Category)) + facet_grid(paste0("rho:",rho) ~ weightname) + labs(x="GDP per capita [kUSD-$(2015)/cap]", y="Welfare index") + ylim(0,1) + scale_color_brewer(palette="BrBG")+ scale_fill_brewer(palette="BrBG")
ggsave(str_glue("figures/Index_GDP_Scatter_for_rho_{year_for_reg_line[1]}.png"), width=8, height = 5)



#add regression line TEMP
ggplot(combined_data %>% filter(year %in% year_for_reg_line & Category!="failed-vetting")) + geom_point(aes(`AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile`, value, color=Category)) + facet_grid(paste0("rho:",rho) ~ weightname) + labs(x="Temperature increase", y="Welfare index", color="Category") + theme(legend.position="bottom") + geom_smooth(aes(`AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile`, value), color="black", method="lm")  + ylim(0,1)  + scale_color_brewer(palette="BrBG")  + theme(legend.position = "right")
ggsave(str_glue("figures/Temp_Welfare_Index_{year_for_reg_line[1]}.png"), width=8, height = 5)

combined_data <- combined_data %>% mutate(gdppc=`GDP|PPP`/Population)
#coefficients
coefficients_temp_welfare <- combined_data %>% filter(year %in% year_for_reg_line & Category!="failed-vetting") %>% nest_by(rho, weightname) %>% mutate(reg = list(lm(value ~ `AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile`, data = data))) %>% reframe(broom::tidy(reg))
openxlsx::write.xlsx(coefficients_temp_welfare, file="reg_temp.xlsx")


#add regression line GDP
ggplot(combined_data %>% filter(year %in% year_for_reg_line & Category!="failed-vetting")) + geom_point(aes(gdppc, value, color=Category)) + facet_grid(paste0("rho:",rho) ~ weightname) + labs(x="GDP per capita [kUSD-$(PPP)/cap]", y="Welfare index", color="Category") + theme(legend.position="bottom") + geom_smooth(aes(gdppc, value), color="black", method="lm")  + ylim(0,1)  + scale_color_brewer(palette="BrBG") + theme(legend.position = "right")
ggsave(str_glue("figures/GDP_Welfare_Index_{year_for_reg_line[1]}.png"), width=8, height = 5)
#coefficients
coefficients_temp_welfare <- combined_data %>% filter(year %in% year_for_reg_line & Category!="failed-vetting") %>% nest_by(rho, weightname) %>% mutate(reg = list(lm(value ~ gdppc, data = data))) %>% reframe(broom::tidy(reg))
openxlsx::write.xlsx(coefficients_temp_welfare, file="reg_gdp.xlsx")





#now all weights
combined_data_allweights <- welfares %>% select(-IMP_marker) %>% left_join(ar6_datadf %>% select(-unit) %>% pivot_wider(names_from = variable, values_from = "value") %>% mutate(gdppc=`GDP|PPP`/Population*1000)) %>% filter(Category!="C8")
combined_data_allweights <- combined_data_allweights %>% mutate(sum_of_weights = across(starts_with("weight_")) %>% rowSums) %>% filter(year %in% year_for_reg_line & Category!="failed-vetting") 

#GDPPC
coefficients_gdppc_welfare_allweights <- combined_data_allweights %>% filter(year %in% year_for_reg_line & Category!="failed-vetting") %>% nest_by(rho, weights) %>% mutate(reg = list(lm(value ~ gdppc, data = data))) %>% reframe(broom::tidy(reg))
#Temperature
coefficients_temp_welfare_allweights <- combined_data_allweights %>% filter(year %in% year_for_reg_line & Category!="failed-vetting") %>% nest_by(rho, weights) %>% mutate(reg = list(lm(value ~ `AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile`, data = data))) %>% reframe(broom::tidy(reg))


#now loop for all variables
p_violin <- list()
for(var in paste0("`",str_subset(names(combined_data_allweights), pattern = "weight_"),"`")[c(1,4,5,6,7,9,11)]){
  print(gsub("`", "", gsub("weight_","",var)))
  #get scenario dataframe of specific normalized weights
  combined_data_allweights_onlyweights <- NULL
  combined_data_allweights_onlyweights <- copy(combined_data_allweights)
  setnames(combined_data_allweights_onlyweights, gsub("`", "", var), "temp_weight")
  combined_data_allweights_onlyweights <- combined_data_allweights_onlyweights %>% ungroup() %>% mutate(weight_report = temp_weight/sum_of_weights) %>% group_by(rho, weights) %>% dplyr::summarise(weight_report = mean(weight_report))
  varnice <-suppressWarnings(plyr::mapvalues(var, from = paste0("`",str_subset(names(combined_data_allweights), pattern = "weight_"),"`")[c(1,4,5,6,7,9,11)], to = c("Temperature", "NOx Emissions", "Sulfur Emissions", "Electricity", "GDP", "Forest Cover", "Food Supply")))
p_violin[[varnice]] <- ggplot(coefficients_temp_welfare_allweights %>% filter(term=="`AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile`") %>% left_join(combined_data_allweights_onlyweights), aes(x = as.factor(rho), y = estimate, color=weight_report)) + geom_violin(draw_quantiles = c(0.5), alpha=0.6, color = "black") +
   labs(x=TeX("$\\rho$"), y="", title = varnice, color ="Weight") + geom_jitter(position = position_jitter(seed = 1, width = 0.1), alpha = 0.4) + geom_hline(yintercept = 0) + theme(legend.position = "bottom") + scale_colour_gradient(low = "yellow", high = "blue", limits = c(0,1))
  ggsave(gsub(" ", "-", gsub("\\|", "-", str_glue('figures/violin_{gsub("`", "", gsub("weight_","",var))}_{year_for_reg_line[1]}.png'))))
}
p_arranged <- ggarrange(p_violin$Temperature, p_violin$GDP, p_violin$`NOx Emissions`, p_violin$`Sulfur Emissions`, p_violin$Electricity, p_violin$`Forest Cover`, p_violin$`Food Supply`, nrow = 4, ncol = 2, common.legend = T, legend = "bottom")
p_arranged <- annotate_figure(p_arranged, left = text_grob("Slope of the Welfare - Temperature relationship (per 1°C)", color = "black", rot = 90))
ggsave(str_glue("figures/arranged_violin_plot_{year_for_reg_line[1]}.pdf"), width = 8, height = 12)

#just a Food Supply plot for the main paper
ggplot(coefficients_temp_welfare_allweights %>% filter(term=="`AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile`") %>% left_join(combined_data_allweights_onlyweights), aes(x = as.factor(rho), y = estimate, color=weight_report)) + geom_violin(draw_quantiles = c(0.5), alpha=0.6, color = "black") +
  labs(x=TeX("$\\rho$"), y="change per 1°C", title = "Slope of the Welfare - Temperature relationship", color ="relative weight of Food Supply") + geom_jitter(position = position_jitter(seed = 1, width = 0.1), alpha = 0.4) + geom_hline(yintercept = 0) + theme(legend.position = "bottom") + scale_colour_gradient(low = "yellow", high = "blue", limits = c(0,1))
ggsave(str_glue("figures/violin_plot_food_supply_for_main_paper_{year_for_reg_line[1]}.pdf"))



