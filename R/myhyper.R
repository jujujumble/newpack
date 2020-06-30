#' Hypergeometric Function
#'
#' Generates a plot of a hypergeometric probability distribution with given data.
#'
#' @param iter
#' @param N
#' @param r
#' @param n
#'
#' @return a plot of the hypergeometric probability distribution using given parameters.
#' @export
#'
#' @examples
#' plot100= myhyper(iter=100,n=5, N=20,r=12)
#'
myhyper = function(iter = 100, N = 20, r = 12, n = 5) {
  # make a matrix to hold the samples initially filled with NA's
  sam.mat = matrix(NA, nrow = n, ncol = iter, byrow = TRUE)

  # Make a vector to hold the number of successes in each trial
  succ = c()

  # Make a table of successes
  for (i in 1:iter) {
    # Fill each column with a new sample
    sam.mat[,i] = sample(rep(c(1,0), c(r, N - r)), n, replace = FALSE)

    # Calculate a statistic from the sample (this case it is the sum)
    succ[i] = sum(sam.mat[,i])
  }

  # Make a table of successes
  succ.tab = table(factor(succ, levels = 0:n))

  # Make a barplot of the proportions
  iter.lab = paste0("iter = ", iter)
  N.lab = paste0("N = ", N)
  r.lab = paste0("r = ", r)
  n.lab = paste0("n = ", n)
  lab = paste(iter.lab, N.lab, r.lab, n.lab, sep = ", ")
  barplot(succ.tab / (iter),  col=hcl.colors(n, palette = "viridis") , main = "HYPERGEOMETRIC simulation", sub = lab, xlab = "Number of successes")
  succ.tab / iter
}


