compute_welfare <- function(indicators, r, w)
{
  # computes the welfare metric C1 of Zuber (2022) for all welfare parameters
  # welfare metric computed for (model, scenario, year, substitution parameter, set of weights)
  # indicators is dataframe with column indicator used for welfare metric
  ##NOT IMPLEMENTED e is list of inequality aversion parameters
  # r is substitutability level 
  # w is list of weights of all variables

  variables=unique(indicators$variable)
  # initiate output with first variable that has non-zero weight to get the correct scenarios in
  w_nonzero <- which(w!= 0)
  # take first non-zero weighted variable 
  var=variables[w_nonzero[1]] 
  out=indicators[indicators$variable==var,]
  
  #rename variable to welfare and add additional parameter rho and weight by which scenario will be identified
  out$rho=r
  out$weight=paste(w, sep=" ", collapse=",")
  out$min<-NULL
  out$max<-NULL
  out$bad<-NULL
  out$log<-NULL
  out$variable="Welfare"
  out$unit="utils"
  out$value= 1*(r==1)+0 #initiate welfare output: start with zero if summation of indicators (rho!=0) and with 1 if geometric product (rho==1)
  
  
  ######## calculate welfare metric ######
  #normalize weights:
  w=w/(sum(w))

	#go through all variables and add value
	
	for (j in w_nonzero)
	{
	  var=variables[j]
	  # reduce data to this vaiable:
	  matrix_var=indicators[indicators$variable==var,]
	  # retrieve values that matches the scenario of consumption  
	  values=matrix_var$indicator[match(out$identifier, matrix_var$identifier)] 
	  # add indicator to previous one 
	  out$value=(r==1)*(out$value*( values )^(w[1,j]))+(1-(r==1))*(out$value+w[1,j]*( values )^(1-r*(r!=1)))
	}
	
	#now add substitutability exponent:
	  out$value=(out$value)^(1/(1-r*(r!=1)))
	
	return(out)
	
}
	