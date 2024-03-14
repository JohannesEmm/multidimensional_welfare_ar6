compute_welfare <- function(all_indicators, r, w)
{
  # computes the welfare metric C1 of Zuber (2022) for all welfare parameters
  # welfare metric computed for (model, scenario, year, substitution parameter, set of weights)
  # indicators is dataframe with column indicator used for welfare metric
  # r is substitutability level 
  # w is list of positive weights
  
  ######## calculate welfare metric ######
  
  #this function returns the welfare metric
  WelfareMetric <- function(v,r,w) {
    (r==1)*prod(( v )^(w), na.rm = FALSE)+(1-(r==1))*(sum(w*( v^(w!=0) )^(1-r*(r!=1)), na.rm = FALSE))^(1/(1-r*(r!=1)))
  }
  out=all_indicators[, by=identifier, value:=WelfareMetric(indicator,r,weight)]
  #reduce to one value per scenario by selecting the first instance of the scenario
  out= out %>%
    filter (variable==w$variable[1])
    
  out$variable="welfare"
  out$unit="utils"
  out$min<-NULL
  out$max<-NULL
  out$bad<-NULL
  out$log<-NULL
  out$value_pc<-NULL
  out$identifier<-NULL
  out$indicator<-NULL
  out$weight<-NULL
  
  
	return(out)
	
}
	