#' TCalc Function
#'
#' @param x vector of a random sample
#'
#' @return The value of the T-Statistic evaluated at the null mean value from a given sample.
#' @export
#'
#' @examples
#' x1 = rnorm(35, mean = 10, sd =1 )
#' mytc(x1)
#'
#'
mytc<- function(x1){


tcalc=(mean(x1)-23)/(sd(x1)/sqrt(30))
tcalc

}
