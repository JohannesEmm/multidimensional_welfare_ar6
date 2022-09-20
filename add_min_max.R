add_min_max <- function (ar6_datadf, variables, variables_pop, variables_min, variables_max)
{
		for(j in 1:length(variables)){	
			var=variables[j]
			name_var=var
  				if (any(variables_pop==var)){# variable name for per capita value 
  				  name_var=paste(var, "|PerCapita", sep="")}
			values=ar6_datadf$value[ar6_datadf$variable==name_var]
			variables_min[j]=na_replace(variables_min[j],min(values, na.rm=TRUE))
			variables_max[j]=na_replace(variables_max[j],max(values, na.rm=TRUE))
			}
	
out=list(mins=variables_min, maxs=variables_max)
return(out)
}



