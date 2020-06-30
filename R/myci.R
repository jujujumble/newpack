
#' Confidence Interval Function
#'
#' @param x vector of values a random sample
#'
#' @return A 95% confidence interval for a given sample/vector, 'x'.
#' @export
#'
#' @examples
#'
myci= function(x){
  t=qt(0.975,24)
  ci=c()
  ci[1]=mean(x)-t*sd(x)/length(x)
  ci[2]=mean(x)+t*sd(x)/length(x)
  return(ci)


}
