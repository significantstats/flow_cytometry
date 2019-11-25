get_neighbours <- function(x, y, size, neigh.sys = "VN1") {

  if (neigh.sys == "VN1") {
    north <- c(x - 1, y)
    south <- c(x + 1, y)
    west <- c(x, y - 1)
    east <- c(x, y + 1)

    if (any(north < 1) || north[1] > size[1] || north[2] > size[2]) {
      north <- c(NA, NA)
    }

    if (any(south < 1) || south[1] > size[1] || south[2] > size[2]) {
      south <- c(NA, NA)
    }

    if (any(west < 1) || west[1] > size[1] || west[2] > size[2]) {
      west <- c(NA, NA)
    }

    if (any(east < 1) || east[1] > size[1] || east[2] > size[2]) {
      east <- c(NA, NA)
    }

    neighbours <- matrix(c(north, south, east, west), ncol = 4, nrow = 2)
    out <- list()
    out$x <- neighbours[1, !is.na(neighbours[1, ])]
    out$y <- neighbours[2, !is.na(neighbours[2, ])]
  }

  return(out)
}
