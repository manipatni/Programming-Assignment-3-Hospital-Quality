best <- function(state, outcome) {
  ## Read outcome data
  
  outcomeData <- read.csv("/home/shailee/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv",stringsAsFactors=FALSE)
  ## Check that state and outcome are valid
  stateValid= state %in% outcomeData$State
  validOutcomeVector <- c("heart attack", "heart failure", "pneumonia")
  outcomeValid= outcome %in% validOutcomeVector
  
  if(!stateValid)
  {
    stop("invalid state")
  }
  
  if(!outcomeValid)
  {
    stop("invalid outcome")
  }
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  outcomeByState <- subset(outcomeData, State==state)
  
  minData <- numeric()
  if(outcome == "heart attack")
  {
     outcomeByMortality <- outcomeByState[[11]]
     outcomeByMortality <- subset(outcomeByMortality, outcomeByMortality !="Not Available")
     minData <- min(as.numeric(outcomeByMortality), na.rm=TRUE)
     outcomeByState <- subset(outcomeByState, outcomeByState[[11]] !="Not Available" )
     outcomeByState <- outcomeByState[-1]
     outcomeByLowMortality <- subset(outcomeByState, as.numeric(outcomeByState[[10]])==minData)
  }
  else if (outcome =="heart failure")
  {
    outcomeByMortality <- outcomeByState[[17]]
    outcomeByMortality <- subset(outcomeByMortality, outcomeByMortality !="Not Available")
    minData <- min(as.numeric(outcomeByMortality), na.rm=TRUE)
    outcomeByState <- subset(outcomeByState, outcomeByState[[17]] !="Not Available" )
    outcomeByState <- outcomeByState[-1]
    outcomeByLowMortality <- subset(outcomeByState, as.numeric(outcomeByState[[16]])==minData)  
  }
  else if(outcome == "pneumonia"){
    outcomeByMortality <- outcomeByState[[23]]
    outcomeByMortality <- subset(outcomeByMortality, outcomeByMortality !="Not Available")
    minData <- min(as.numeric(outcomeByMortality), na.rm=TRUE)
    outcomeByState <- subset(outcomeByState, outcomeByState[[23]] !="Not Available" )
    outcomeByState <- outcomeByState[-1]
    outcomeByLowMortality <- subset(outcomeByState, as.numeric(outcomeByState[[22]])==minData)
  }
  
  outcomeByLowMortality$Hospital.Name
 
}
