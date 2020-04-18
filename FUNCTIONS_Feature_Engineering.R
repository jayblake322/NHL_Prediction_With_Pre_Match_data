# Feature engineering Functions

cumSumResetOtherColumnValue <- function(df, col1, col2, value) {
  
  # this scrip calculates the cummulative sum of one column based on the values of another column.
  # resets the calculation every time a stipulated value occurs
  
  cummulative <- vector()
  x <- 1
  
  for (i in 1:nrow(df)){
    if (df[i, col1] == value){
      
      cummulative[i] <- 0  
      x <- i +1
      
    } else {
      
      cummulative[i] <- sum(df[x:i, col2])
      
    }
  }
  
  return(cummulative)
  
}
numberDaysSinceEvent <- function(df, eventCol, dateCol, event1, event2, upperLimit, lowerLimit){
  # This function calculates the number of days past in a date column based on a binominal factor value occuring in another column
  # to deal with the first values if the comparison date doesn't occure first the funciton takes limits to remove wrong values with NA
  
  daysSinceEvent <- vector()
  obs <- nrow(df)
  x <- 1
  
  for (i in 1:obs){
    
    if (df[i, eventCol] == event1) {
      
      x <- i
      daysSinceEvent[i] <- 0
      
    } else if (df[i, eventCol] == event2){
      
      daysSinceEvent[i]  <- df[i, dateCol] - df[x, dateCol]
      
    }   
    
  }
  
  daysSinceEvent[daysSinceEvent > upperLimit | daysSinceEvent < lowerLimit] <- NA
  daysSinceEvent <- unlist(daysSinceEvent)
  return(daysSinceEvent)
  
}
offset_column <- function(df, col, offset){
  # this function moves every value in a df column to the right index by the number of spaces indicated by offset. Initial NAs. 
  
  offset_vec <- vector()
  n <- nrow(df)
  loop_start <- offset + 1 
  
  for (x in loop_start:n){
    
    y = x - offset
    offset_vec[x] = df[y, col]
    
  } 
  
  return(offset_vec)
  
}
resetCumSum = function(x, value){
  # set to restart the cummulative sum when a certain value is encountered in the same column
  cs = cumsum(x)
  cs - cummax((x == value) * cs)
}
rollingCumSum <- function(df, col, n) {
  # Rolling Cummulative Sum Function by Stipulated Number of Values
  # takes a dataframe, the name of the column on which to base the cumsum formula and,
  # the number of trailing values to be included in the sum
  
  
  rollingSum <- vector()
  nPlusOne <- n +1
  
  for(i in nPlusOne:nrow(df)) {
    
    # initialise vector and trailing index
    
    m <- i - n 
    
    # calculate the rolling sum to the trailing index
    rollingSum[i] <- sum(df[1:i, col]) - sum(df[1:m, col])
    
  }
  
  # fill the missing values
  for (b in 1:n){
    
    rollingSum[b] <- sum(df[1:b, col])
    
  }
  
  return(rollingSum)
  
}
rollingCumSumOffset <- function(df, col, n) {
  # Rolling Cummulative Sum Function by Stipulated Number of Values
  # takes a dataframe, the name of the column on which to base the cumsum formula and,
  # the number of trailing values to be included in the sum
  # until 'n' in the rolling cumsum result vector, the values are summed 
  # cum sum is on values indexed to be 1 before 'n' so they are therefore previous to the nth obs
  # the first value in the returned vector is set to 0 as there are no values preceding this index
  
  
  rollingSum <- vector()
  nPlusOne <- n +1
  
  for(i in nPlusOne:nrow(df)) {
    
    # initialise vector and trailing index
    
    m <- i - n - 1
    offsetOne <- i - 1
    
    # calculate the rolling sum to the trailing index
    rollingSum[i] <- sum(df[1:offsetOne, col]) - sum(df[1:m, col])
    
  }
  
  # fill the missing values up until 'n'
  for (b in 2:nPlusOne){
    
    c <- b - 1     
    rollingSum[b] <- sum(df[1:c, col])
    
  }
  
  rollingSum[1] <- 0
  
  return(rollingSum)
  
}




