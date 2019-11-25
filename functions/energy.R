energy <- function(x, neigh.sys = "VN1") {

  size.x <- dim(x)
  ergy <- 0

  for (n in 1:size.x[1]) {
    for (m in 1:size.x[2]) {
      if (neigh.sys == "VN1") {
        neighbours <- get_neighbours(x = n, y = m, size = size.x,
                                     neigh.sys = neigh.sys)
      }
      neighbours <- neighbours$x + (neighbours$y - 1) * size.x[1]
      energy.node <- (2 * x[n, m] - 1) * sum(2 * x[neighbours] - 1)
      ergy <- ergy + energy.node
    }
  }

  ergy <- -ergy
  return(ergy)
}
