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

####---------------------- Covariance of Corner Nodes ----------------------####
  
  covariance_c1 <- Vectorize( function(theta) {
    cov_c1 <- theta * (1 - theta) * (128 * theta ^ 2 - 128 * theta + 40)
    return(cov_c1)
  }, vectorize.args = "theta")
  
  covariance_c2 <- Vectorize( function(theta) {
    cov_c2 <- theta * (1 - theta) * (160 * theta ^ 2 - 160 * theta + 48)
    return(cov_c2)
  }, vectorize.args = "theta")
  
  covariance_c3 <- Vectorize( function(theta) {
    cov_c3 <- theta * (1 - theta) * (192 * theta ^ 2 - 192 * theta + 56)
    return(cov_c3)  
  }, vectorize.args = "theta")

####----------------------- Covariance of Edge Nodes -----------------------####
  
  covariance_e1 <- Vectorize( function(theta) {
    cov_e1 <- theta * (1 - theta) * (272 * theta ^ 2 - 272 * theta + 80)
    return(cov_e1)
  }, vectorize.args = "theta")
  
  covariance_e2 <- Vectorize( function(theta) {
    cov_e2 <- theta * (1 - theta) * (304 * theta ^ 2 - 304 * theta + 88)
    return(cov_e2)
  }, vectorize.args = "theta")
  
  covariance_e3 <- Vectorize( function(theta) {
    cov_e3 <- theta * (1 - theta) * (336 * theta ^ 2 - 336 * theta + 96)
    return(cov_e3)
  }, vectorize.args = "theta")
  
  covariance_e4 <- Vectorize( function(theta) {
    cov_e4 <- theta * (1 - theta) * (368 * theta ^ 2 - 368 * theta + 104)
    return(cov_e4)
  }, vectorize.args = "theta")
  
####---------------------- Covariance of Inner Nodes -----------------------####
  
  covariance_i1 <- Vectorize( function(theta) {
    cov_i1 <- theta * (1 - theta) * (512 * theta ^ 2 - 512 * theta + 144)
    return(cov_i1)
  }, vectorize.args = "theta")
  
  covariance_i2 <- Vectorize( function(theta) {
    cov_i2 <- theta * (1 - theta) * (544 * theta ^ 2 - 544 * theta + 152)
    return(cov_i2)
  }, vectorize.args = "theta")
  
  covariance_i3 <- Vectorize( function(theta) {
    cov_i3 <- theta * (1 - theta) * (576 * theta ^ 2 - 576 * theta + 160)
    return(cov_i3)
  }, vectorize.args = "theta")
  
  covariance_i4 <- Vectorize( function(theta) {
    cov_i4 <- theta * (1 - theta) * (608 * theta ^ 2 - 608 * theta + 168)
    return(cov_i4)
  }, vectorize.args = "theta")
  
  covariance_i5 <- Vectorize( function(theta) {
    cov_i5 <- theta * (1 - theta) * (640 * theta ^ 2 - 640 * theta + 176)
    return(cov_i5)
  }, vectorize.args = "theta")