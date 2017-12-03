rankall <- function(outcomeChr, rankObj = "best") {

  outcomeData <-  read.csv("/home/shailee/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv",stringsAsFactors=FALSE)
  
  tableData <- data.frame(State = names(tapply(outcomeData$State, outcomeData$State, 
                                              length)), Freq = tapply(outcomeData$State, outcomeData$State, length))
  rownames(tableData) <- NULL
  
  ## Check that state and outcome are valid
  validOutcomeVector <- c("heart attack", "heart failure", "pneumonia")
  outcomeValid= outcomeChr %in% validOutcomeVector
  sortData <- numeric()
  
  if(!outcomeValid)
  {
    stop("invalid outcome")
  }
  
  nameChr <- character(0)
  inputDfr <- data.frame(Outcome = c("heart attack", "heart failure", "pneumonia"), 
                         Col = c(11, 17, 23))
  
  for (stateChr in tableData$State) {
    stateData <- outcomeData[outcomeData$State == stateChr, ]
    colNum <- inputDfr[inputDfr$Outcome == outcomeChr, 2]
    stateData <- stateData[complete.cases(stateData[, colNum]), ]
    stateData <- stateData[order(stateData[, colNum], stateData$Hospital.Name), 
                         ]

    if (rankObj == "best") 
      rankNum <- 1 
    else if (rankObj == "worst") 
        rankNum <- nrow(stateData) 
    else
      rankNum <- as.numeric(rankObj)
    
    nameChr <- c(nameChr, stateData[rankNum, ]$Hospital.Name)

  }
  
  return(data.frame(hospital = nameChr, state = tableData$State))
}
