resetCumSum = function(x, value){
  # set to restart the cummulative sum when a certain value is encountered in the same column
  cs = cumsum(x)
  cs - cummax((x == value) * cs)
}