markov.trans.1 <- function(x) {
  # Determine the first-order markov chain for x
  # 
  # Args:
  #   x: a vector of the sequence of transitions from one state to another
  # 
  # Returns:
  #   A transition matrix
  
  num.states = max(x)
  p <- array(0, c(num.states, num.states))
  
  for (t in 1:(length(x) - 1)) p[x[t], x[t + 1]] <- p[x[t], x[t + 1]] + 1
  for (i in 1:num.states) p[i, ] <- p[i, ] / sum(p[i, ])
  
  colnames(p) <- c(1:num.states)
  rownames(p) <- c(1:num.states)
  
  return(p)
}