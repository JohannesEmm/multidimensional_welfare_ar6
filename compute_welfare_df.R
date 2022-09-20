compute_welfare_df <- function(ar6_datadf, variables, variables_pop, variables_min, variables_max, variables_bad, rho, weight)
{
# computes the welfare metric C1 of Zuber (2022) for all welfare parameters
# welfare metric computed for (model, scenario, year, substitution parameter, weight)
# ar6_datadf is dataframe
# variables contains list of variables to be included in welfare metric
# variables_pop is list of variables that have been normalized by population
# variables_min is list of minimum values for variables
# variables_max is list of maximum values for variables
# variables_bad is list of variables that are a bad 
##NOT IMPLEMENTED e is list of inequality aversion parameters
# rho is list of substitutability levels 
# weight is list of relative weights of consumption to all other variables
	
	#initiate output
	out=ar6_datadf[1,]
	out$rho=NA
	out$weight=NA
	out$variable="Welfare"
	out$unit="utils"

	#go through welfare parameters and compute welfare metric
		for (r in rho){
				   for (w in weight){
								out_local=compute_welfare(ar6_datadf, variables, variables_pop, variables_min, variables_max, variables_bad, r, w)
							  out=rbind(out, out_local)
							   }
				  }
#remove first line
out<- out[-1,]

return(out)

}



