# this plots the welfare data, run 'main.R' first
# saves in subfolder "figures"

#dir.create("figures")

# Labels
weight.labs <- c("weight:high GDP", "weight:equal", "weight:low GDP")
names(weight.labs) <- c(paste(weights[1][[1]], sep=" ", collapse=","), paste(weights[2][[1]], sep=" ", collapse=","), paste(weights[3][[1]], sep=" ", collapse=","))
rho.labs <- c("rho:0", "rho:1", "rho:5")
names(rho.labs) <- c("0", "1", "5")

# Creating a summary figure displaying the mean and 95% confidence interval for selected FaIRv1.6.2 categories

welfares$year <- as.numeric(welfares$year)

df_welfare_summary <- filter(welfares, year >=2020 , value != "") %>%
  group_by(year,Category_FaIRv1.6.2,rho,weight) %>%
  summarise(n=n(),mean=mean(value),max=max(value),min=min(value),sd = sd(value))%>%
  mutate(sem = sd / sqrt(n - 1),
         CI_lower = mean + qt((1-0.95)/2, n - 1) * sem,
         CI_upper = mean - qt((1-0.95)/2, n - 1) * sem)

df_welfare_summary_selected <- subset(df_welfare_summary,Category_FaIRv1.6.2=='C1a'|Category_FaIRv1.6.2=='C1b'|Category_FaIRv1.6.2=='C2'|Category_FaIRv1.6.2=='C3'|Category_FaIRv1.6.2=='C4'|Category_FaIRv1.6.2=='C5'|Category_FaIRv1.6.2=='C7')

png(file = "Summary-Figure.png") 	
fig=ggplot(df_welfare_summary_selected,aes(x=year,y=mean,color=Category_FaIRv1.6.2)) +
  geom_line(aes(x=year,y=mean,color=Category_FaIRv1.6.2))+
  geom_ribbon(aes(ymin=CI_lower,ymax=CI_upper,fill=Category_FaIRv1.6.2),color="grey70",alpha=0.4)+
  facet_grid( rho ~ weight, labeller=labeller(rho = rho.labs, weight = weight.labs))
print(fig)
dev.off()

#subplot per model, rho and weight

models=unique(welfares$model)
#	for( m in models){	
#data_m <- filter(welfares, year != "", value != "") %>%
#	  	dplyr::filter(Category!="failed-vetting")%>% 
#  		filter(!is.na(value)) %>%
#  		filter(!is.na(model), model == m) 
#     data_m$value<-as.numeric(data_m$value) 
#		#remove "/" from model name for saving
#			name=str_replace_all(string=m, pattern="/", repl="_")
#			png(file = paste("figures/",name,".png",sep="")) 	
#		#plot if welfare levels available
#		 			if(dim(data_m)[1]>0){
#				p= 	ggplot(data_m ) +
#					geom_line(aes(year, value, group=interaction(scenario), color=Category)) + 
# 					labs(title = paste(m, ": welfare metric by rho and weight"),
#       				y = "welfare", x = "") + 
#  					facet_grid( rho ~ weights, labeller=labeller(rho = rho.labs, weights = weight.labs), scales="free")
#				print(p) 
#			}
#		dev.off()
#	}
# all scenarios
theme_set(theme_bw())
png(file = paste("figures/","AR6_database- all scenarios - new variables",".png",sep="")) 
data_m <- filter(welfares, year != "", value != "") %>%
  dplyr::filter(Category!="failed-vetting")%>% 
  filter(!is.na(value)) %>%
  filter(!is.na(model), model %in% models) 
data_m$value<-as.numeric(data_m$value)
p=(ggplot(data_m ) +
  geom_line(aes(year, value, group=interaction(model,scenario), color=Category), alpha = 0.7) + 
  labs(title = paste("AR6-database", ": welfare metric by rho and weight"),
       y = "welfare", x = "") + 
    scale_x_continuous(breaks = seq(from = 2000, to = 2099, by = 50))+
  facet_grid( rho ~ weights, scales = "free", labeller=labeller(rho = rho.labs, weights = weight.labs)))
print(p)
dev.off()

# all scenarios
theme_set(theme_bw())
png(file = paste("figures/","AR6_database- mean across categories - new variables",".png",sep="")) 
data_m <- welfares %>% group_by(Category, year, rho, weights) %>%
  dplyr::summarize(Mean = mean(value, na.rm=TRUE), SD=sd(value, na.rm=TRUE))
data_m<- data_m %>% 
  drop_na(Mean, SD)
data_m <- data_m %>% 
  filter(year %in% seq(from = 2010, to = 2100, by = 10))%>%
  dplyr::filter(Category!="failed-vetting")
p=(ggplot(data_m, aes(x = year, group = Category)) +
     geom_line(aes(y=Mean, color=Category), size=0.7) + 
     geom_ribbon(aes(y = Mean, ymin = Mean - SD, ymax = Mean + SD, fill = Category), alpha = .1)+
     labs(title = paste("AR6-database", ": welfare metric by rho and weight"),
          y = "welfare", x = "") + 
     scale_x_continuous(breaks = seq(from = 2000, to = 2099, by = 50))+
     facet_grid( rho ~ weights, scales = "free", labeller=labeller(rho = rho.labs, weights = weight.labs)))
print(p)
dev.off()



