
## Function for giving back rank results from hospital data
# Takes in desired state, desired outcome type and rank number
# State should be in list of states, outcome can be one of ["heart attack","heart failure", "pneumonia"],
#rank can be an integer less then tha number of hospitals, or "best", or "worst"
#
# Gives back the alphabetical first name amongst the hospitals with with the appropriate rank

rankhospital <- function(state, outcome, num = "best"){
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
  
  # examine num parameter, substitute if neded
  reverse<-FALSE
  if(num=="best"){
    num<-1
  }
  else if(num=="worst"){
    num<-1
    reverse<-TRUE
  }
  else if(num>nrow(data)){
    return(NA)
  } 
  
  # do the magic, order the data, choose the name
  # two cases if reverse or not
  if(!reverse){
    ordered<-data[order(data[,outcol],data[,2]),]
    names<-ordered[num,2]
  }
  else{
    ordered<-data[order(-data[,outcol],data[,2]),]
    names<-ordered[num,2]  
  }
  
  return(names)
}