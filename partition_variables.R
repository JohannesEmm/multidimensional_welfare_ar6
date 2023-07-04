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
  #second loop assign medium and low weight to partition of (vars_with_pos_weights-high subset) into 
  #two further subsets
  
  #initiating list of weights:
  all_weights <- matrix(ncol=length(red_vars), nrow=1)
  
  #number of partitions of two subsets:
  n_2_high=2^(length(red_vars) )-1 #number of possible subsets containing at least one variable
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
  #removing duplicates
  normalized_weights=unique(normalized_weights)
  colnames(normalized_weights)<-red_vars
  
  #if both nox and sox emissions are present:
  #adding Nox variable with half the weight:
  
  if(yes_snox==1)
  {
    final_weights=normalized_weights
    
    id=which(colnames(final_weights)=="Emissions|Sulfur")
    final_weights[,id]=normalized_weights[,id]/2
    final_weights=as.data.frame(final_weights)
    final_weights[,"Emissions|NOx"]<- normalized_weights[,id]/2
  }else
  {final_weights=as.data.frame(normalized_weights)}
  
  #add all variables with zero weight that are not in vars_with_pos_weights but are in variables
  final_weights[,variables[!(variables %in% vars_with_pos_weights)]]<-0
  
  return(final_weights)
  
}

 