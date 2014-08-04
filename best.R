best<-function(state, outcome){
 
  ## Read outcome data
  data<-read.csv("outcome-of-care-measures.csv",na.strings="Not Available")
  
  outcome<-gsub(" ", ".", outcome)
  coln<-colnames(data)
  
  ##Check that state and outcome are valid
  
  if (!any(data$State == state)){  
    stop("Invalid State")   
  }
  
  len<-length(grep(outcome, coln, ignore.case=TRUE))
  if ( len == 0){ 
    stop("Invalid outcome")
    }
  
  ##return hospital name with lowest 30-day death rate in state
  data.s<-data[data$State==state,]
  search<-paste(c("^Hospital.30.Day.Death..Mortality..Rates.from"),outcome, sep=".")
  col<-as.integer(grep(search,colnames(data),ignore.case=TRUE))
  
  as.character(data.s[which.min(data.s[,col]),2])
  
}