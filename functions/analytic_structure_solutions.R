####------------------- Expected Value of Lattice Energy -------------------####  

  expected_value <- Vectorize( function(theta, N, M) {
    exp_val <- -(4 * N * M - 2 * N - 2 * M) * (2 * theta - 1) ^ 2
    return(exp_val)
  }, vectorize.args = "theta")

####---------------------- Sum of the node variances -----------------------####

  variance_nodes_sum <- Vectorize( function(theta, N, M) {
    var_nodes <- (16 * N * M - 14 * N - 14 * M + 8) * (2 * theta - 1) ^ 2
    var_nodes <- var_nodes + (4 * N * M - 2 * N - 2 * M)
    var_nodes <- 4 * theta * (1 - theta) * var_nodes
    return(var_nodes)
  }, vectorize.args = "theta")

####----------------------- Covariance of Stucture 1 -----------------------####

  covariance_s1 <- Vectorize( function(theta, n1, n2) {
    cov_s1 <- theta * (1 - theta) * (4 * (1 + n1 + n2) * (2 * theta - 1) ^ 2 + 4)
    return(cov_s1)
  }, vectorize.args = "theta")

####----------------------- Covariance of Stucture 2 -----------------------####

  covariance_s2 <- Vectorize( function(theta) {
    cov_s2 <- theta * (1 - theta) * (4 * (2 * theta - 1) ^ 2)
    return(cov_s2)
  }, vectorize.args = "theta")

####----------------------- Covariance of Stucture 3 -----------------------####

  covariance_s3 <- Vectorize( function(theta) {
    cov_s3 <- theta * (1 - theta) * (8 * (2 * theta - 1) ^ 2)
    return(cov_s3)
  }, vectorize.args = "theta")
