add_per_capita_df <- function (ar6_datadf, variables_pop)
{
# adds per-capita values for all variables in variables_pop to ar6_datadf
out=ar6_datadf; 

# retrieve population data
matrix_pop=out[out$variable=="Population",]

for (j in 1:length(variables_pop))
	{
	var=variables_pop[j]

	# add new variable with per capita value
	# reduce data to this vaiable:
	matrix_var=out[out$variable==var,]

	# create data with per capita values
	name_var_pc=paste(var, "|PerCapita", sep="")
	out_var=matrix_var
	out_var$variable=name_var_pc
	out_var$unit=paste(unique(matrix_var$unit), "/", unique(matrix_pop$unit), sep="")

	#calculate per capita values:
	out_var$value=matrix_var$value/matrix_pop$value[match(matrix_var$identifier, matrix_pop$identifier)] 

	#bind new data to old data
	out=rbind(out, out_var)
	}


return(out)
}



