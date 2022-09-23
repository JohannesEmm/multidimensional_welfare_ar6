compute_welfare_df <- function(indicators, rho, weight)
{
  # computes the welfare metric C1 of Zuber (2022) for all welfare parameters
  # welfare metric computed for (model, scenario, year, substitution parameter, set of weights)
  # indicators is dataframe with column indicator used for welfare metric
  ##NOT IMPLEMENTED e is list of inequality aversion parameters
  # rho is list of substitutability levels 
  # weight is dataframe with rows set of weights of all variables
	
	#initiate output
	out=indicators[1,]
	out$min<-NULL
	out$max<-NULL
	out$bad<-NULL
	out$log<-NULL
	out$rho=NA
	out$weight=NA
	out$variable="Welfare"
	out$unit="utils"
	
  
	#go through welfare parameters and compute welfare metric
		for (r in rho){
				   for (w in 1:dim(weight)[1]){
								out_local=compute_welfare(indicators, r, weight[w,])
							  out=rbind(out, out_local)
							   }
				  }
#remove first line
out<- out[-1,]

return(out)

}



