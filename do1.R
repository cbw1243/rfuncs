# Generate data with two covariates, one binary treatment, a continuous outcome.
# Treatment selection is based on a log model. 
generateData<- function(n, pscore.type = 1) {
  
  # Generate baseline covariates:
  W1 <- runif(n, min = 0.02, max = 0.7)  
  W2 <- rnorm(n, mean = (0.2 + 0.125*W1), sd = 1) 
  
  # Generate binary treatment indicator
  if (pscore.type == 1) {
    A <- rbinom(n, 1, plogis(-.5 + .5*W1 + .1*I(W1^2) + .5*I(W1*W2) - .5*W2))
  } else if (pscore.type == 2) {
    A <- rbinom(n, 1, plogis(-.7 + 1.8*W1 -.1*I(W1^2) + 1.7*I(W1*W2) - 1.4*W2))
  } else if (pscore.type == 3) {
    A <- rbinom(n, 1, plogis(-.3 + 2*W1 -2*I(W1^2) + 2*I(W1*W2) - 2.5*W2))
  }
  
  # Generate the potential outcomes
  Y.0 <- rnorm(n, mean = (-.5 + 2*poly(W1,2)  - W2), sd=1)
  Y.1 <- rnorm(n, mean = (-.5 + 2*poly(W1,2) - W2 + 1.5 + 2*poly(W1,2) - W2), sd=1)
  
  # Generate the observed outcome
  Y <-  ifelse(A == 1, Y.1, Y.0)
  
  return(data.frame(W1,W2,A,Y,Y.1,Y.0))
}