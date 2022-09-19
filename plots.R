# this plots the welfare data, run 'main.R' first
# saves in subfolder "figures"

dir.create("figures")

#subplot per model, rho and weight
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
  					facet_grid( rho ~ weight, labeller=label_both)
				print(p) 
			}
		dev.off()
	}


