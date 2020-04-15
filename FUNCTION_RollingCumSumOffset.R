# Rolling Cummulative Sum Function by Stipulated Number of Values

rollingCumSumOffset <- function(df, col, n) {
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


