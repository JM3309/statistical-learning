#"""

# This is the function for markov chains that simulates a Markov chain X(t) 
# until the first time the chain is in state s, 
# assuming X(0) = s0. The function should return the path of
# the chain from t = 0 to when it "hits" state s. You may use
# your language's discrete sampler (in R sample) or write your own.

#"""


MarkovChain <- function(p,s0,s){
  x <- s0 
  path <- x
  n <- nrow(p)  #the num of states
  
  while(x != s){
    new_state <- sample(x = rep(1:n),size=1,prob = p[x,])
    path <- c(path,new_state)
    x <- new_state
  }
  return(path) 
}


#sample of markov chain
p1 <- matrix(c(0.1,0.5,0.4,0.1,0.9,0,0.3,0.3,0.4),nrow=3,ncol=3)

MarkovChain(p1,2,3) # my chain starts at 2,ends at 3

