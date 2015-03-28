## Function for giving back best result from hospital data
# Takes in desired state and desired outcome type
# State should be in list of states and outcome can be one of ["heart attack","heart failure", "pneumonia"]
#
# Gives back the alphabetical first name amongst the hospitals with the minimum of outcome value 

best <- function(state, outcome){
  # Read data
  data <- read.csv("~/course-work/Assignment3/outcome-of-care-measures.csv", colClasses = "character")
  
  #Check for correct outcome parameter
  valid_outcomes<-c("heart attack","heart failure", "pneumonia")
  if( !outcome %in% valid_outcomes){
    stop("invalid outcome")
  }
  
  # Check for correct state parameter
  if(!state %in% data[,7]){
    stop("invalid state")
  }
  
  # rough solution for mapping outcomes to colums, could be more nice 
  outcol<-0
  if(outcome=="heart attack") { 
    outcol <- 11
  }
  if(outcome=="heart failure") { 
    outcol <- 17
  }
  if(outcome=="pneumonia") { 
    outcol <- 23
  }
  
  # Choose subset based on state
  data<-data[data[,7]==state,]
  
  # We load it as text, not as factors. Sadly needs conversion. :-(
  suppressWarnings(data[,outcol]<-as.numeric(data[,outcol]))
  
  #do the magic: get the proper name
  name<-data[which(data[,outcol]==min(data[,outcol], na.rm=TRUE) ),2]
  
  # sort alpha and choose
  result<-sort(name)[1]
  return(result)
}
