#' mynewcurve
#' Lab 6 Function
#'
#' @param mu mean
#' @param sigma
#' @param a
#'
#' @return
#' @export
#'
#' @examples
#'
#' plot(table(rpois(100, 5)), type = "h", col = "red", lwd = 10,
#' main = "rpois(100, lambda = 5)")
#' myncurve(mu = 5, sigma = 3, a = 0.05)

myncurve = function(mu, sigma,a){
  curve(dnorm(x,mean=mu,sd = sigma), xlim = c(mu - 3*sigma, mu + 3*sigma))

  xcurve = seq(mu-3*sigma ,a, length = 1000)
  ycurve = dnorm(xcurve, mu, sigma)

  polygon(c(-Inf,xcurve, a), c(0, ycurve, 0), col = "maroon4")

  prob = pnorm(a, mu, sigma )
  prob= round(prob,4)
  list(prob)

}
