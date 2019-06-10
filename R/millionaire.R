#' Functions to calculate how soon you'll be a millionaire
#'
#' These functions will make you rich
#' goal: how many days to reach your goal
#' days: how much money after x days
#' @param m start money
#' @param p daily percentage profit - default: 2
#' @param g goal, default 1 million
#' @param d days of trading
#' @keywords money
#' @export
#' @examples
#' goal(5000,2,100000)
#' days(100,2,5000)

goal <- function(m, p = 2, g = 1000000) {
  n <- 0
  while(m < g) {
    m <- m + m*p/100
    n <- n+1
  }
  return(n)
}

days <- function(d, p = 2, m = 7000) {
  n <- 0
  while(n < d) {
    m <- m + m*p/100
    n <- n+1 }
  return(m)
}
