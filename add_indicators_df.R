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
  # df variables' values as in ar6_datadf but replaces value for variables in variables_pop with per-capita level 
  df=out$value
  # retrieve population data
  matrix_pop=out[out$variable=="Population",]
  values=df/matrix_pop$value[match(out$identifier, matrix_pop$identifier)] 
  df[which(out$variable %in% variables_pop)]=values[which(out$variable %in% variables_pop)]
  

# find minima and maxima
  
  min_df=tapply(X=df, INDEX=out$variable, FUN=min, na.rm=TRUE )
  max_df=tapply(X=df, INDEX=out$variable, FUN=max, na.rm=TRUE )
  variables_min[which(is.na(variables_min))]=min_df[which(is.na(variables_min))]
  variables_max[which(is.na(variables_max))]=max_df[which(is.na(variables_max))]
  
# add column to out that indicates variable's (i) min (ii) max, whether variables is (iii) bad (iv) logged
  variables=unique(out$variable)
  out$min=rep(NA, dim(out)[1])
  out$max=rep(NA, dim(out)[1])
  for(j in 1:length(variables)){	
    var=variables[j]
    out$min[out$variable==var]=rep(variables_min[j], length(which(out$variable==var)))
    out$max[out$variable==var]=rep(variables_max[j], length(which(out$variable==var)))
  }
  
  out$bad= 1*(out$variable %in% variables_bad)+0;
  out$log= 1*(out$variable %in% variables_log)+0;
  
  # remove all data points with variable below minimum 
  vec=df-out$min<0
  df<-df[-which(vec)]
  out<-out[-which(vec),]
  
# compute indicator
  # first for variables that are logged
  out$indicator[out$log==1] <- (log(df[out$log==1])-log(out$min[out$log==1]) )/(log(out$max[out$log==1])-log(out$min[out$log==1])) 
  
  # now for all other variables that are bads:
  vec=(out$bad==1 & out$log==0)
  out$indicator[vec]=  (out$max[vec]-df[vec]) /(out$max[vec]-out$min[vec])
  # rest of variables: 
  vec=(out$bad==0 & out$log==0)
  out$indicator[vec]=(df[vec]-out$min[vec]) /(out$max[vec]-out$min[vec])
  
return(out)
}



