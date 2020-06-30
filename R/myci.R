
#' Confidence Interval Function
#'
#' @param x vector of values a random sample
#'
#' @return A 95% confidence interval for a given sample/vector, 'x'.
#' @export
#'
#' @examples
#'x = rnorm(35, mean = 0, sd =1)
#' myci(x)
myci= function(x,n = 25 ){
  t=qt(0.975,(1-n)
  ci=c()
  ci[1]=mean(x)-t*sd(x)/length(x)
  ci[2]=mean(x)+t*sd(x)/length(x)
  return(ci)


}
