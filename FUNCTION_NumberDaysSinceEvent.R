
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


