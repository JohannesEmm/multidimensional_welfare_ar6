add_min_max <- function (ar6_datadf, variables, variables_pop, variables_min, variables_max)
{
	#get minimum values
		for(j in 1:length(variables)){	
			if(variables_min[j]=="NA"){
			var=variables[j]
  				if (any(variables_pop==var))
				{
					# variable name for per capita value
					name_var_pc=paste(var, "|PerCapita", sep="")
					values=ar6_datadf$value[ar6_datadf$variable==name_var_pc]
					variables_min[j]=min(values, na.rm=TRUE)
				}else{
					values=ar6_datadf$value[ar6_datadf$variable==var]
					variables_min[j]=min(values, na.rm=TRUE)
				}
			}
		}

	#get maximum values
		for(j in 1:length(variables))
		{	
			if(variables_max[j]=="NA")
			{
			var=variables[j]
  				if (any(variables_pop==var))
				{
					# variable name for per capita value
					name_var_pc=paste(var, "|PerCapita", sep="")
					values=ar6_datadf$value[ar6_datadf$variable==name_var_pc]
					variables_max[j]=max(values, na.rm=TRUE)
				}else	
				{
					values=ar6_datadf$value[ar6_datadf$variable==var]
					variables_max[j]=max(values, na.rm=TRUE)
				}
			}
		}
out=list(mins=variables_min, maxs=variables_max)
return(out)
}



