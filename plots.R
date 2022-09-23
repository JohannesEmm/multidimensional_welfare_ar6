# this plots the welfare data, run 'main.R' first
# saves in subfolder "figures"

dir.create("figures")

#subplot per model, rho and weight
weight.labs <- c("weight:high GDP", "weight:equal", "weight:low GDP")
names(weight.labs) <- c("1,0,0,0.5,0.5,100,0,1,0", "1,0,0,0.5,0.5,1,0,1,0", "1,0,0,0.5,0.5,0.2,0,1,0")
rho.labs <- c("rho:0", "rho:1", "rho:2")
names(rho.labs) <- c("0", "1", "2")


models=unique(welfares$model)
	for( m in models){	
		data_m <- filter(welfares, year != "", value != "") %>%
		dplyr::filter(Vetting_future=="Pass" & Vetting_historical=="Pass")%>% 
  		filter(!is.na(value)) %>%
  		filter(!is.na(model), model == m) 
		data_m$value<-as.numeric(data_m$value) 
		#remove "/" from model name for saving
			name=str_replace_all(string=m, pattern="/", repl="_")
			png(file = paste("figures/",name,".png",sep="")) 	
		#plot if welfare levels available
		 			if(dim(data_m)[1]>0){
				p= 	ggplot(data_m ) +
					geom_line(aes(year, value, group=interaction(scenario), color=Category_FaIRv1.6.2)) + 
  					labs(title = paste(m, ": welfare metric by rho and weight"),
       				y = "welfare", x = "") + 
  					facet_grid( rho ~ weight, labeller=labeller(rho = rho.labs, weight = weight.labs))
				print(p) 
			}
		dev.off()
	}
# all scenarios
png(file = paste("figures/","AR6_database",".png",sep="")) 
data_m <- filter(welfares, year != "", value != "") %>%
  dplyr::filter(Vetting_future=="Pass" & Vetting_historical=="Pass")%>% 
  filter(!is.na(value)) %>%
  filter(!is.na(model), model %in% models[10:17]) 
data_m$value<-as.numeric(data_m$value)
p=ggplot(data_m ) +
  geom_line(aes(year, value, group=interaction(scenario), color=Category_FaIRv1.6.2)) + 
  labs(title = paste("AR6-database", ": welfare metric by rho and weight"),
       y = "welfare", x = "") + 
  facet_grid( rho ~ weight, labeller=labeller(rho = rho.labs, weight = weight.labs))
print(p)
dev.off()
#
data_m <- filter(ar6_datadf, model=="AIM/CGE 2.2", scenario=="EN_NPi2020_600", year==2020) 
data_a <- filter(ar6_datadf, variable=="AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile") 
data_a <- filter(data_a, value<0) 

