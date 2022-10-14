compute_welfare_df <- function(indicators, rho, weights)
{
  # computes the welfare metric C1 of Zuber (2022) for all welfare parameters
  # welfare metric computed for (model, scenario, year, substitution parameter, set of weights)
  # indicators is dataframe with column indicator used for welfare metric
  ##NOT IMPLEMENTED e is list of inequality aversion parameters
  # rho is list of substitutability levels 
  # weight is dataframe with variable and corresponding weight
  
  
  #prepare output 
  out=indicators[1,]
  out$variable="welfare"
  out$unit="utils"
  out$min<-NULL
  out$max<-NULL
  out$bad<-NULL
  out$log<-NULL
  out$value_pc<-NULL
  out$identifier<-NULL
  out$indicator<-NULL
  out$rho=NA
  out$weights=NA
  
  #this function return TRUE if the specific list of variables contains all 
  #variables with positive weight
  MatchVariables <- function(vars,var_list) {
    all(var_list %in% vars)
  }
  
  #go through welfare parameters and compute welfare metric
  for (w in 1:length(weights)[1]){
    ws=weights[w][[1]];
   #reduce number of scenarios to those which have all variables with positive weight:
    #convert weights to dataframe to merge with  all_indicators 
    weight_loc=data.frame(variable=names(ws), weight=unlist(ws))
    #normalize weights:
    weight_loc$weight=weight_loc$weight/sum(weight_loc$weight)
    #reduce to positive weights
    weight_loc = weight_loc %>%
      filter(weight>0)
    #get only scenarios that have all variables with possible weight
    all_indicators=data.table(indicators, key="variable")
    all_indicators=all_indicators[, by=identifier, FullList:=MatchVariables(variable,weight_loc$variable)]
    all_indicators=all_indicators[FullList=="TRUE"]
    all_indicators$FullList<-NULL
    
    #reduce to indicators with only positive weights
    weight_loc=data.table(weight_loc, key="variable")
    all_indicators=all_indicators[weight_loc]
    #create column with weights
    all_indicators$weights=paste(ws, sep=" ", collapse=",")
    
    for (r in rho){
      all_indicators$rho=r
      out_local=compute_welfare(all_indicators, r, weight_loc)
      out=rbind(out, out_local)
    }
  }
  #remove first line
  out<- out[-1,]
  
  return(out)
  
}



