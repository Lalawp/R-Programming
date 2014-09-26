best <- function(state, outcome){
  ##determines if outcome is a valid input
  validoutcome <- c("heart attack", "heart failure", "pneumonia")
  if(outcome %in% validoutcome == FALSE) {
    stop("Invalid outcome")
  }
  
  
  
  ##read outcome data
  setwd("C:/Users/Jim/Documents/Data Science/R Programming") 
  files_full <- list.files()
  outcomedata <- read.csv("outcome-of-care-measures.csv")
  
  ##simplify data
  outcomedata <- outcomedata[c(2, 7, 11, 17, 23)]
  names(outcomedata)[1] <- "name"
  names(outcomedata)[2] <- "state"
  names(outcomedata)[3] <- "heart attack"
  names(outcomedata)[4] <- "heart failure"
  names(outcomedata)[5] <- "pneumonia"
  
  ##determines if state is a valid input
  validstates <- unique(outcomedata[, 2])
  if(state %in% validstates == FALSE) {
    stop("Invalid state")
  } 
  
  
  ##isolates requested state data and removes NAs
  outcomedata <- outcomedata[outcomedata$state==state & outcomedata[outcome] != "Not Available", ]
  
  ##ranks data by inputed outcome
  outcomedata <- outcomedata[order(outcomedata[[outcome]]), ]
  ##returns the best hospital in a state regarding outcome
  return(head(outcomedata))    
} 
