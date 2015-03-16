#just in case
source("complete.R")

corr <- function(directory, threshold = 0) {
	## inicialize return vector
	valu <- vector(mode="numeric")
	## call complete function to get complete count and file id-s
	completes <- complete(directory)
	## check if over threshold
	validids <- completes[completes[,2] > threshold,1]
	## iterate through valid files
	for(i in validids) {
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
		## do the magic aka. the correlation :-) use only complete rows
		valu<- c(valu,cor(monitordata$sulfate,monitordata$nitrate,use="complete.obs"))
	}
	## just to see console output nicely
	return(valu)	
}
