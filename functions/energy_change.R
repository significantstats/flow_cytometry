energy_change <- function(energy.pre, energy.post, index) {
  
  above <- index - 1
  below <- index + 1
  left <- index - nrow(energy.pre)
  right <- index + nrow(energy.pre)
  
  energy.stay <- energy.pre[index]
  energy.swap <- energy.post[index]
  
  if (above > 0 && above <= length(energy.pre)) {
    energy.stay <- energy.stay + energy.pre[above]
    energy.swap <- energy.swap + energy.post[above]
  }
  
  if (below > 0 && below <= length(energy.pre)) {
    energy.stay <- energy.stay + energy.pre[below]
    energy.swap <- energy.swap + energy.post[below]
  }
  
  if (left > 0 && left <= length(energy.pre)) {
    energy.stay <- energy.stay + energy.pre[left]
    energy.swap <- energy.swap + energy.post[left]
  }
  
  if (right > 0 && right <= length(energy.pre)) {
    energy.stay <- energy.stay + energy.pre[right]
    energy.swap <- energy.swap + energy.post[right]
  }
  
  energy.change <- energy.stay - energy.swap
  
  return(energy.change)
}