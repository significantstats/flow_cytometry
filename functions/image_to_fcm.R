image_to_fcm <- function(x, original, min = 0, max = 1023) {
  
  index.x <- which(x != 0)
  groups.x <- x[which(x != 0)]
  index.original <- (original[,1] + 1) + (max - min + 1) * original[,2]
  
  groups <- rep(0, times = nrow(original))
  for (i in 1:length(index.x)) {
    groups[which(index.original == index.x[i])] <- groups.x[i]
  }
  
  remove.small.groups <- as.numeric(names(which(table(groups) <= 10)))
  
  for (i in 1:length(groups)) {
    if (groups[i] %in% remove.small.groups) {
      groups[i] <- 0
    }
  }
  
  groups.orig <- groups
  
  group.numbering <- sort(unique(groups))
  group.numbering.new <- 0:(length(group.numbering - 1))
  
  for (i in 1:length(groups)) {
    groups[i] <- group.numbering.new[which(group.numbering == groups[i])]
  }
  
  return(list(groups = groups, groups.orig = groups.orig))
}