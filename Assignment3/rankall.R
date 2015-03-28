## Function for giving back rank results from hospital data of different states
# Takes in desired outcome type and rank number
# Outcome can be one of ["heart attack","heart failure", "pneumonia"],
# rank can be an integer less then tha number of hospitals, or "best", or "worst"
#
# Gives back the alphabetical first name amongst the hospitals with the appropriate rank for each state 

rankall <- function(outcome, num = "best"){
  # Read data
  data <- read.csv("~/course-work/Assignment3/outcome-of-care-measures.csv", colClasses = "character")
  
  #Check for correct outcome parameter
  valid_outcomes<-c("heart attack","heart failure", "pneumonia")
  if( !outcome %in% valid_outcomes){
    stop("invalid outcome")
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
  
  # We load it as text, not as factors. Sadly needs conversion. :-(
  suppressWarnings(data[,outcol]<-as.numeric(data[,outcol]))
  
  # split by state
  splitted<<-split(data,data[,7])
  #print(str(splitted))
  #return()
  
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
  
  names<-matrix(,0,2)
  colnames(names)<-c("hospital","state")
  
  statenames<-names(splitted)
  iterator<-0
  for(statedata in splitted){
    iterator<-iterator+1
    
    # do the magic, order the data, choose the name
    # two cases if reverse or not
    if(!reverse){
      ordered<-statedata[order(statedata[,outcol],statedata[,2]),]
      out<-c(ordered[num,2],statenames[iterator])
      out.name=statenames[iterator]
      names<-rbind(names,out)
    }
    else{
      ordered<-statedata[order(-statedata[,outcol],statedata[,2]),]
      out<-c(ordered[num,2],statenames[iterator])
      names<-rbind(names,out)
    }
  }
  rownames(names)<-names[,2]
  return(names)
}