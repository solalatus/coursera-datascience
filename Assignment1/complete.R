complete <- function(directory, id = 1:332) {
	## loop through the id vector
	for(i in id) {
		## concatenate to achieve appropriate filenames
		prefix <- ""
		if(i < 100) {
			prefix<- "0"
		}
		if(i < 10) {
			prefix<-"00"
		}
		filename <- paste(directory,"/",prefix,toString(i[1]),".csv", sep="")
		## read single monitor data
		monitordata <- read.csv(filename)
		# create result matrix first, or add result vector to matrix	
		if(exists("valu")){	
			valu<-rbind(valu,c(i,nrow(na.omit(monitordata))))
		}
		else {
			valu<-matrix(ncol=2)			
			valu[1,] <- c(i,nrow(na.omit(monitordata)))
		}
	}
	## fancy column names
	colnames(valu) <- c("id","nobs")
	## calling explicit return, so I can easily see output on console
	return(valu)
}
