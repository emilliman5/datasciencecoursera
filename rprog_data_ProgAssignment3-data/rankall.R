rankall<-function(outcome, num="best"){
 
  ## Read outcome data
  data<-read.csv("outcome-of-care-measures.csv",na.strings="Not Available")
  
  outcome<-gsub(" ", ".", outcome)
  coln<-colnames(data)
  
  ##Check that state and outcome are valid
  
  len<-length(grep(outcome, coln, ignore.case=TRUE))
  if ( len == 0){ 
    stop("Invalid outcome")
    }
  
  ##return hospital name with lowest 30-day death rate in state
  if(num=="best"){
      num<-1
  }
  search<-paste(c("^Hospital.30.Day.Death..Mortality..Rates.from"),outcome, sep=".")
  col<-as.integer(grep(search,colnames(data),ignore.case=TRUE))
  
  data.s<-subset(data, (!is.na(data[,col])))
 
  if(num=="worst"){
    num<-length(data.s$State)
  }
  as.character(data.s[ order(data.s[col]), ][num,2])
    
}