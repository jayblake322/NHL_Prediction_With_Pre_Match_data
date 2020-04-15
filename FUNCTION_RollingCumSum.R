# Rolling Cummulative Sum Function by Stipulated Number of Values

rollingCumSum <- function(df, col, n) {
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


