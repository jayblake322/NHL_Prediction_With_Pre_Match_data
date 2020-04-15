
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
