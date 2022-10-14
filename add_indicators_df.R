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
  
  #out=merge(out, mins, by = c("variable"))
  #out=merge(out, maxs, by = c("variable"))

  out$bad= 1*(out$variable %in% variables_bad)+0;
  out$log= 1*(out$variable %in% variables_log)+0;
  
  # remove all data points with variable below minimum 
  vec=out$value_pc-out$min<0
  out<-out[-which(vec),]
  
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



