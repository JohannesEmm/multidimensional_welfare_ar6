compute_welfare <- function(ar6_datadf, variables, variables_pop, variables_min, variables_max, variables_bad, r, w)
{
# computes the welfare metric C1 of Zuber (2022)
# welfare metric computed for (model, scenario, year, substitution parameter, weight)
# ar6_datadf is dataframe
# variables contains list of variables to be included in welfare metric
# variables_pop is list of variables that have been normalized by population
# variables_min is list of minimum values for variables
# variables_max is list of maximum values for variables
# variables_bad is list of variables that are a bad 
##NOT IMPLEMENTED e is list of inequality aversion parameters
# r is substitutability levels 
# w is relative weight of consumption to all other variables

	#convert minima and maxima to numeric values
	variables_min=as.numeric(variables_min)
	variables_max=as.numeric(variables_max)

	#calculate weights: this assumes that consumption is the first variable!
	w_c= w/(w+length(variables)-1) # weight of consumption metric so that its weights is "w" times the weight of each of the other variables
	w_nc=1/(w+length(variables)-1) # individual weight of all other variables

	######## calculate welfare metric ######
	
	# take first variable (consumption) 
	# consumption welfare indicator is different by transforming with natural logarithm

		var=variables[1] 
       	# variable name for per capita value
		name_var=paste(var, "|PerCapita", sep="")
		matrix_cons=ar6_datadf[ar6_datadf$variable==name_var,]

	######## calculate welfare metric ######
		out=matrix_cons
		#rename variable to welfare and add additional parameter rho and weight by which scenario will be identified
			out$rho=r
			out$weight=w
			out$variable="Welfare"
			out$unit=""
			
		#calculate first summand of welfare metric:
			#use geometric mean for r=1
			if (r==1){
				out$value=(  (log(out$value)-log(variables_min[1]))/(log(variables_max[1])-log(variables_min[1]))  )				
				}else{
				out$value=w_c*(  (log(out$value)-log(variables_min[1]))/(log(variables_max[1])-log(variables_min[1]))  )^(1-r)
				}

		#go through all other variables and add value
		
		for (j in 2:length(variables))
			{
			var=variables[j]
				if (any(variables_pop==var))
				{
					# variable name for per capita value
					var=paste(var, "|PerCapita", sep="")
				}
			# reduce data to this vaiable:
			matrix_var=ar6_datadf[ar6_datadf$variable==var,]
			# retrieve values that matches the scenario of consumption  
			values=matrix_var$value[match(out$identifier, matrix_var$identifier)] 
			# add indicator to previous one
				if (r ==1){				
					if( any(variables_bad==var)){#use indicator as a bad
					out$value=out$value*( (variables_max[j]-values)/(variables_max[j]-variables_min[j]) )
					}else{#use indicator as a good
					out$value=out$value+w_nc*( (values-variables_min[j])/(variables_max[j]-variables_min[j]) )^(1-r)
					}

				}else{
				if( any(variables_bad==var)){#use indicator as a bad
					out$value=out$value+w_nc*( (variables_max[j]-values)/(variables_max[j]-variables_min[j]) )^(1-r)
					}else{#use indicator as a good
					out$value=out$value+w_nc*( (values-variables_min[j])/(variables_max[j]-variables_min[j]) )^(1-r)
					}
				}
			}

		#now add substitutability exponent:
			if (r==1){
				out$value=(out$value)^(1/3)
				}else{
				out$value=(out$value)^(1/(1-r))
				}
			


return(out)

}



