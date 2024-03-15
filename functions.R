partition_variables <- function(variables, vars_with_pos_weights, rel_weights, yes_snox)
{
  #function partitions vars_with_pos_weights into all possible combinations of 3 subsets
  #3 subsets each have weights in rel_weights (these are denoted high,
  #medium and low and taken as the order in rel_weights but the actual order does not matter)
  #yes_snox specifies that variables sox and nox will be taken together as 1 variable that each receive half the weight
  #finally all variables in variables but not in vars_with_pos_weights get a zero weight
  
  #define weights:
  weight_high=rel_weights[1];
  weight_med=rel_weights[2];
  weight_low=rel_weights[3];
  
  #if both nox and sox emissions are in set of variables, remove 1
  if(yes_snox==1)
  {
    id=which(vars_with_pos_weights=="Emissions|NOx")
    red_vars=vars_with_pos_weights[-id]
  }else
  {red_vars=vars_with_pos_weights}
  
  #creating two loops: first loop runs over subset out of vars_with_pos_weights with high weight, 
  #second loop assigns medium and low weight to partition of (vars_with_pos_weights-high subset) into 
  #two further subsets
  
  #initiating list of weights:
  all_weights <- matrix(ncol=length(red_vars), nrow=1)
  
  #number of partitions of two subsets:
  n_2_high=2^(length(red_vars) )-1  #number of possible subsets containing at least one variable
  for (high_id in 0:n_2_high)       #adding empty set to have this as well
  {
    #initiate weights with zeros:
    weights=1:length(red_vars)*0;
    #initiate subset with medium and low weights
    subset_high=c();
    
    id = high_id;
    for(var_name in red_vars)
    {
      i=which(red_vars == var_name);
      pwr = 2^(length(red_vars)-i);                   #/* convert variable to bit */
      if(id >= pwr)                                   #/* is bit in subset with high weight? */
      {
        weights[i] = weight_high;                     
        id = id - pwr;
      }else{
        subset_high=c(subset_high, red_vars[i])
      }
    }
    n_2_med_low=2^(length(subset_high))-1
    if(n_2_med_low>0)
    {  for (med_id in 0:n_2_med_low)                   
    {
      id2=med_id
      for(var_name in subset_high)
      {
        j=which(red_vars == var_name);
        i=which(subset_high == var_name);
        pwr = 2^(length(subset_high)-i);                #/* convert variable to bit */
        if(id2 >= pwr)                                  #/* is bit in subset with high weight? */
        {
          weights[j] = weight_med;                     
          id2 = id2 - pwr;
        }else{
          weights[j] = weight_low; 
          
        }
        
      }
      all_weights=rbind(all_weights,weights)
    }
    }
    
  }
  all_weights<-all_weights[-1,]
  #removing all rows that have zeros:
  row_sub = apply(all_weights, 1, function(row) all(row !=0 ))
  ##Subset as usual
  all_weights=all_weights[row_sub,]
  #normalizing
  normalized_weights=all_weights/(rowSums(all_weights))*1
  #round to avoid numerical imprecision:
  normalized_weights=round(normalized_weights, digits=14)
  #removing duplicates
  row_sub=!duplicated(normalized_weights)
  ##Subset as usual
  all_weights=all_weights[row_sub,]
  
  colnames(all_weights)<-red_vars
  
  #if both nox and sox emissions are present:
  #adding Nox variable with half the weight:
  
  if(yes_snox==1)
  {
    final_weights=all_weights
    
    id=which(colnames(final_weights)=="Emissions|Sulfur")
    final_weights[,id]=final_weights[,id]/2
    final_weights=as.data.frame(final_weights)
    final_weights[,"Emissions|NOx"]<- final_weights[,id]
  }else
  {final_weights=as.data.frame(all_weights)}
  
  #add all variables with zero weight that are not in vars_with_pos_weights but are in variables
  final_weights[,variables[!(variables %in% vars_with_pos_weights)]]<-0
  
  return(final_weights)
  
}


add_indicators_df <- function (ar6_datadf, variables_log, variables_bad, variables_pop, variables_min, variables_max)
{
  # add indicators for all variables
  # if !variables_log: indicator is (variable - min_variable)/(max_variable-min_variable) for a good 
  #                                 (max_variable - variable)/(max_variable-min_variable) for a bad
  # if variables_log: indicator is (log(variable) - log(min_variable))/(log(max_variable)-log(min_variable))
  
  # initiate output as ar6_datadf because indicator is calculated for each variable 
  # out has same data as ar6_datadf + columns for: min, max, bad and log for each variable + level of indicator for each variable
  
  out=ar6_datadf; 
  
  # convert per-capita variables:
  out %>% add_column(value_pc = NA)
  out$value_pc=out$value
  # retrieve population data
  matrix_pop=out[out$variable=="Population",]
  values=out$value/matrix_pop$value[match(out$identifier, matrix_pop$identifier)] 
  #replace variables to be converted with per-capita value
  out$value_pc[which(out$variable %in% variables_pop)]=values[which(out$variable %in% variables_pop)]
  
  # convert forest land cover to share:
  out <- out %>% left_join(out %>% filter(variable=="Land Cover") %>% select(-variable,-unit,-value_pc) %>% rename(land_cover=value))
  out <- out %>% mutate(forest_share=value/land_cover)
  out$value_pc <- ifelse(out$variable =="Land Cover|Forest" , out$forest_share,out$value_pc)
  
  #delete added columns
  out$land_cover<-NULL
  out$forest_share<-NULL
  
  #remove outliers (check script "CheckOutliers.R")
  #remove all values of population size that is bigger than 100000 million 
  #(two models have this for four scenarios in total) :
  out<-out[-which(out$variable=="Population" & out$value > 1*10^5),]
  #
  #remove all values of GPD and consumption below 0.1
  #(many scenarios removed) :
  out<-out[-which(out$variable=="Consumption" & out$value_pc <0.1),]
  out<-out[-which(out$variable=="GDP|PPP" & out$value_pc <0.1),]
  #
  #remove all values of GPD and consumption below 0.1
  #(many scenarios removed) :
  out<-out[-which(out$variable=="Final Energy|Electricity" & out$value_pc <10^(-4)),]
  #
  #remove all values of food demand size above 10000 kcal/pc/day
  #(many scenarios removed) :
  out<-out[-which(out$variable=="Food Demand" & out$value > 10000),]
  #
  #remove all values with zero food energy supply
  #(many scenarios removed) :
  out<-out[-which(out$variable=="Food Energy Supply" & out$value == 0),]
  #
  #remove all values with low land cover
  #(many scenarios removed) :
  out<-out[-which(out$variable=="Land Cover" & out$value < 10000),]
  #
  #remove all values with low forest land cover
  #(many scenarios removed) :
  out<-out[-which(out$variable=="Land Cover|Forest" & out$value < 2000),]
  
  
  
  # find minima and maxima
  
  min_df=tapply(X=out$value_pc, INDEX=out$variable, FUN=min, na.rm=TRUE )
  max_df=tapply(X=out$value_pc, INDEX=out$variable, FUN=max, na.rm=TRUE )
  #order both list by alphabet:
  min_df = min_df[order(names(min_df))]
  max_df = max_df[order(names(max_df))]
  variables_min = variables_min[order(names(variables_min))]
  variables_max = variables_max[order(names(variables_max))]
  
  variables_min[which(is.na(variables_min))]=min_df[which(is.na(variables_min))]
  variables_max[which(is.na(variables_max))]=max_df[which(is.na(variables_max))]
  
  #convert minima and maxima to dataframe to merge with  out 
  mins=data.frame(variable=names(variables_min), min=unlist(variables_min))
  maxs=data.frame(variable=names(variables_max), max=unlist(variables_max))
  
  # add column to out that indicates variable's (i) min (ii) max, whether variables is (iii) bad (iv) logged
  out=data.table(out, key="variable")
  mins=data.table(mins, key="variable")
  maxs=data.table(maxs, key="variable")
  out=out[mins]
  out=out[maxs]
  
  out$bad= 1*(out$variable %in% variables_bad)+0;
  out$log= 1*(out$variable %in% variables_log)+0;
  
  # remove all data points with variable below minimum 
  out<- out  %>% filter (!(value_pc-min<0) )
  
  # compute indicator
  # first for variables that are logged
  out$indicator[out$log==1] <- (log(out$value_pc[out$log==1])-log(out$min[out$log==1]) )/(log(out$max[out$log==1])-log(out$min[out$log==1])) 
  
  # now for all other variables that are bads:
  vec=(out$bad==1 & out$log==0)
  out$indicator[vec]=  (out$max[vec]-out$value_pc[vec]) /(out$max[vec]-out$min[vec])
  # rest of variables: 
  vec=(out$bad==0 & out$log==0)
  out$indicator[vec]=(out$value_pc[vec]-out$min[vec]) /(out$max[vec]-out$min[vec])
  
  return(out)
}




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



compute_welfare_df <- function(indicators, rho, weights)
{
  # computes the welfare metric C1 of Zuber (2022) for all welfare parameters
  # welfare metric computed for (model, scenario, year, substitution parameter, set of weights)
  # indicators is dataframe with column indicator used for welfare metric
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
  #add column for weight in each scenario
  weight_columns=paste("weight",colnames(weights), sep="_")
  out[, weight_columns]=NA
  
  #this function returns TRUE if the specific list of variables contains all 
  #variables with positive weight
  MatchVariables <- function(vars,var_list) {
    all(var_list %in% vars)
  }
  
  #go through welfare parameters and compute welfare metric
  for (w in 1:dim(weights)[1]){
    ws=weights[w,];
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
    all_indicators[,weight_columns]=ws
    
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




