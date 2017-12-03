rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcomeData <- read.csv("/home/shailee/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv",stringsAsFactors=FALSE)
  
  ## Check that state and outcome are valid
  stateValid= state %in% outcomeData$State
  validOutcomeVector <- c("heart attack", "heart failure", "pneumonia")
  outcomeValid= outcome %in% validOutcomeVector
  sortData <- numeric()

  if(!stateValid)
  {
    stop("invalid state")
  }
  
  if(!outcomeValid)
  {
    stop("invalid outcome")
  }
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  outcomeByState <- subset(outcomeData, State==state)
  
  if(outcome == "heart attack")
  {
    outcomeByState <- subset(outcomeByState, outcomeByState[[11]] !="Not Available" )
    outcomeByMortality <- outcomeByState[order(as.numeric(outcomeByState[[11]]),na.last=NA),]
    uniqueMortality <- unique(outcomeByMortality[[11]])
    r <- nrow(outcomeByMortality)
    outcomeFinal <- as.data.frame(matrix(data=NA,nrow=r,ncol=2))
    names(outcomeFinal) <- c("HospitalNames", "Mortality")
    i <-1
    for(uM in uniqueMortality)
    {
      
      outcomeByUniqueMortality <- subset(outcomeByMortality, outcomeByMortality[[11]] ==uM )
      outcomeBySortedUniqueMortality <- outcomeByUniqueMortality[order(as.character(outcomeByUniqueMortality[[2]]),na.last=NA),] 
      
      for(hospNames in outcomeBySortedUniqueMortality[[2]] )
      {
        outcomeFinal$HospitalNames[i] <- hospNames
        outcomeFinal$Mortality[i] <- uM
        i <- i+1
      } 
    }
    
  }
  else if (outcome =="heart failure")
  {
    outcomeByState <- subset(outcomeByState, outcomeByState[[17]] !="Not Available" )
    outcomeByMortality <- outcomeByState[order(as.numeric(outcomeByState[[17]]),na.last=NA),]
    uniqueMortality <- unique(outcomeByMortality[[17]])
    r <- nrow(outcomeByMortality)
    outcomeFinal <- as.data.frame(matrix(data=NA,nrow=r,ncol=2))
    names(outcomeFinal) <- c("HospitalNames", "Mortality")
    i <-1
    for(uM in uniqueMortality)
    {
       
        outcomeByUniqueMortality <- subset(outcomeByMortality, outcomeByMortality[[17]] ==uM )
        outcomeBySortedUniqueMortality <- outcomeByUniqueMortality[order(as.character(outcomeByUniqueMortality[[2]]),na.last=NA),] 
       
        for(hospNames in outcomeBySortedUniqueMortality[[2]] )
        {
          outcomeFinal$HospitalNames[i] <- hospNames
          outcomeFinal$Mortality[i] <- uM
          i <- i+1
        } 
    }
    
  }
  else if(outcome == "pneumonia"){
  
    outcomeByState <- subset(outcomeByState, outcomeByState[[23]] !="Not Available" )
    outcomeByMortality <- outcomeByState[order(as.numeric(outcomeByState[[23]]),na.last=NA),]
    uniqueMortality <- unique(outcomeByMortality[[23]])
    r <- nrow(outcomeByMortality)
    outcomeFinal <- as.data.frame(matrix(data=NA,nrow=r,ncol=2))
    names(outcomeFinal) <- c("HospitalNames", "Mortality")
    i <-1
    for(uM in uniqueMortality)
    {
      
      outcomeByUniqueMortality <- subset(outcomeByMortality, outcomeByMortality[[23]] ==uM )
      outcomeBySortedUniqueMortality <- outcomeByUniqueMortality[order(as.character(outcomeByUniqueMortality[[2]]),na.last=NA),] 
      
      for(hospNames in outcomeBySortedUniqueMortality[[2]] )
      {
        outcomeFinal$HospitalNames[i] <- hospNames
        outcomeFinal$Mortality[i] <- uM
        i <- i+1
      } 
    }
    
  }
  
  if(num == "worst")
  {
    num <- r
  }
  else if (num =="best")
  {
    num <- 1
  }
  
  outcomeFinal[num,]$HospitalNames
}
