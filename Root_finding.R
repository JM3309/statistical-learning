##############################################################################################

# 1-d root finding

# EX: f(x) = x^2 − a
##############################################################################################

#######################
# 1.using newton method
#######################

MySqrt_newton <- function(a,epsilon=10^-6) {
  x <- 2
  f <- function(x) x^2-a
  df <- function(x) 2*x
  iteration<- 1
  while(abs(f(x))>epsilon){
  x <- x - (f(x)/df(x))
  iteration <- iteration + 1
  }
return(list(x=x,error=1000-x^2,iteration=iteration))
}

MySqrt_newton(1000)

###########################
# 2. using bisection method
###########################

MySqrt_bisection<- function(a, tol=10^-6) {
# we know sqrt(a) is between 0 and a, so use that as first interval
xL <- 0
xR <- a
f <- function(x) x^2 - a
iter <- 0
repeat {
  xM <- (xL+xR)/2
  if (f(xM)*f(xR) > 0)
  {
    xR <- xM
    }
  else
  {
    xL <- xM
      }
iter <- iter + 1
if (abs(f(xM))<tol)
 break
}
return (list(x=xM, error=1000-xM^2, iter=iter))
}

MySqrt_bisection(1000)

################################
# 3. use the R function: uniroot 
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/uniroot
################################

MySqrt_uniroot <-function(a,tol=10^-6) {
  f <-function(x) x^2-a
  out <- uniroot(f,c(0,a),tol=tol)
  x<- out$root
  iteration <- out$iter
  return(list(x=x,error=1000-x^2,iteration=iteration))
}

MySqrt_uniroot(1000)
