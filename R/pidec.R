#' Function to calculate pi
#'
#' Algorithm: Gregory-Leibniz
#' Note that results are incorrect beyond 15 decimals (R uses finite precision arithmetic)
#' @param i number of iterations in the G-L series
#' @keywords pi gregory-leibniz
#' @export
#' @examples
#' print(pidec(2000), digits = 15)

pidec <- function(i) {
# declare variable
x <- 0

# loop for iterations
for (n in seq(2,i*2,2)) {x[n-1] <- (n*(n+1)*(n+2))}

# remove NAs
x <- x[!is.na(x)]

# calculations
y <- 4/x
for (n in seq(2,i,2)) {y[n] <- y[n]*(-1)}
pi <- 3 + sum(y)
return(pi)
}
