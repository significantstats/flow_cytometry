#' The Generalised Binomial Distribution
#'
#' @name GenBinomial
#'
#' @description Density, distribution function, quantile function and random
#'     generation for the generalised binomial distribution with parameters
#'     \code{pi}, \code{theta} and \code{size}.
#'
#' @param g vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations.
#' @param pi probability of success of the bernoulli.
#' @param theta probability of success on each binomial trial.
#' @param size number of trials.
#' @param lower.tail logical; if TRUE (default), probabilities are
#' \eqn{Pr[X \leq x]}{Pr[X \le x]}, otherwise \eqn{Pr[X > x]}{Pr[X > x]}.
#'
#' @details The generalised binomial distribution with \code{size = n},
#'     \code{pi = pi} and \code{theta = theta} has density
#'     \deqn{Pr(G = g) = \begin{cases} x, g = 0 \\ y, g > 0\end{cases}}
#'
#' @aliases dgenbinom pgenbinom qgenbinom rgenbinom

## Density Function ##
#' @export

dgenbinom <- Vectorize( function(g, pi, theta, size) {

  if (!is.numeric(g)) {
    stop('g must be an integer valued input')
  } else if (floor(g) != g) {
    stop('g must be an integer valued input')
  }

  if (missing(pi)) {
    stop('pi is missing and is a required input')
  } else if (!is.numeric(pi)) {
    stop('pi must be a numeric argument')
  } else if (pi < 0 | pi > 1) {
    stop('pi must be a probability')
  }

  if (missing(theta)) {
    stop('theta is missing and is a required input')
  } else if (!is.numeric(theta)) {
    stop('theta must be a numeric argument')
  } else if (theta < 0 | theta > 1) {
    stop('theta must be a probability')
  }

  if (missing(size)) {
    stop('size is missing and is a required input')
  } else if (!is.numeric(size)) {
    stop('size must be an integer valued input')
  } else if (size < 1 | floor(size) != size) {
    stop('size must be greater than 1')
  }

  if (g < 0) {
    pmf <- 0
  } else if (g > size) {
    pmf <- 0
  } else if (g == 0) {
    pmf <- (1 - theta) ^ size + (1 - pi) * sum(dbinom(x = 1:size, size = size,
                                                      prob = theta))
  } else {
    pmf <- pi * dbinom(x = g, size = size, prob = theta)
  }

  return(pmf)
}, vectorize.args = "g")


## Distribution Function ##
#' @rdname GenBinomial
#' @export

pgenbinom <- Vectorize( function(g, pi, theta, size, lower.tail = TRUE) {

  if (!is.numeric(g)) {
    stop('g must be an integer valued input')
  } else if (floor(g) != g) {
    stop('g must be an integer valued input')
  }

  if (missing(pi)) {
    stop('pi is missing and is a required input')
  } else if (!is.numeric(pi)) {
    stop('pi must be a numeric argument')
  } else if (pi < 0 | pi > 1) {
    stop('pi must be a probability')
  }

  if (missing(theta)) {
    stop('theta is missing and is a required input')
  } else if (!is.numeric(theta)) {
    stop('theta must be a numeric argument')
  } else if (theta < 0 | theta > 1) {
    stop('theta must be a probability')
  }

  if (missing(size)) {
    stop('size is missing and is a required input')
  } else if (!is.numeric(size)) {
    stop('size must be an integer valued input')
  } else if (size < 1 | floor(size) != size) {
    stop('size must be greater than 1')
  }

  if (!is.logical(lower.tail)) {
    stop('lower.tail must be either TRUE or FALSE')
  }

  if (g < 0) {
    cdf <- 0
  } else if (g >= size) {
    cdf <- 1
  } else {
    cdf <- sum(dgenbinom(g = 0:floor(g), pi = pi, theta = theta, size = size))
  }

  if (lower.tail == FALSE) {
    cdf <- 1 - cdf
  }

  return(cdf)
}, vectorize.args = "g")


## Quantile Function ##
#' @rdname GenBinomial
#' @export

qgenbinom <- Vectorize( function(p, pi, theta, size, lower.tail = TRUE) {

  if (!is.numeric(p)) {
    stop('p must be a numeric argument')
  } else if (p < 0 | p > 1) {
    stop('p must be a probability')
  }

  if (missing(pi)) {
    stop('pi is missing and is a required input')
  } else if (!is.numeric(pi)) {
    stop('pi must be a numeric argument')
  } else if (pi < 0 | pi > 1) {
    stop('pi must be a probability')
  }

  if (missing(theta)) {
    stop('theta is missing and is a required input')
  } else if (!is.numeric(theta)) {
    stop('theta must be a numeric argument')
  } else if (theta < 0 | theta > 1) {
    stop('theta must be a probability')
  }

  if (missing(size)) {
    stop('size is missing and is a required input')
  } else if (!is.numeric(size)) {
    stop('size must be an integer valued input')
  } else if (size < 1 | floor(size) != size) {
    stop('size must be greater than 1')
  }

  if (!is.logical(lower.tail)) {
    stop('lower.tail must be either TRUE or FALSE')
  }

  if (lower.tail == FALSE) {
    p <- 1 - p
  }

  if (p <= 0) {
    quant <- 0
  } else if (p >= 1) {
    quant <- size
  } else {
    cdf <- pgenbinom(g = 0:size, pi = pi, theta = theta, size = size)
    quant <- which(cdf >= p)[[1]] - 1
  }

  return(quant)
}, vectorize.args = "p")


## Random Generator ##
#' @rdname GenBinomial
#' @export

rgenbinom <- function(n, pi, theta, size) {

  if (missing(n)) {
    stop('n is missing and is a required input')
  } else if (!is.numeric(n)) {
    stop('n must be an integer valued input')
  } else if (floor(n) != n | n < 1) {
    stop('n must be an integer valued input greater than 0')
  }

  if (missing(pi)) {
    stop('pi is missing and is a required input')
  } else if (!is.numeric(pi)) {
    stop('pi must be a numeric argument')
  } else if (pi < 0 | pi > 1) {
    stop('pi must be a probability')
  }

  if (missing(theta)) {
    stop('theta is missing and is a required input')
  } else if (!is.numeric(theta)) {
    stop('theta must be a numeric argument')
  } else if (theta < 0 | theta > 1) {
    stop('theta must be a probability')
  }

  if (missing(size)) {
    stop('size is missing and is a required input')
  } else if (!is.numeric(size)) {
    stop('size must be an integer valued input')
  } else if (size < 1 | floor(size) != size) {
    stop('size must be greater than 1')
  }

  probs <- dgenbinom(g = 0:size, pi = pi, theta = theta, size = size)
  outcomes <- seq(from = 0, to = size, by = 1)

  rand <- sample(x = outcomes, prob = probs, size = n, replace = TRUE)

  return(rand)
}
