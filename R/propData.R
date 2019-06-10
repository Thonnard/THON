#' Function to calculate proportions in tables/dataframes
#' 
#' This function uses frequencies as input and generates proportions (by row or column) as output
#' @param data table/dataframe (no tibbles)
#' @param byRow calculate proportions by row? Default is TRUE
#' @param round number of decimal places
#' @keywords proportions
#' @export
#' @examples
#' propData(mytable, byRow=TRUE)
#' propData(mytable, byRow=FALSE)

propData <- function(data, byRow=TRUE, round=2) {
  dataTemp <- data
  for (i in 1:nrow(data)) {
    for (y in 1:ncol(data)) {
      if(isTRUE(byRow)) {
        dataTemp[i,y] <- round(data[i,y]/sum(data[i,])*100,round)
        }
      else {
        dataTemp[i,y] <- round(data[i,y]/sum(data[,y])*100,round)}
        }
  }
  return(dataTemp)
}
