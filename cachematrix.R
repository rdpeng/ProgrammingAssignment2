# This assignment is not as easy as it looks like. But I tried my best:



## what I want to do here is to present a function which enables me to get the inverse of x, automatically and taking advantage of cache which makes the calculation faster.

makeCacheMatrix <- function(x = matrix()) {
m<-NULL
set<-function(y)
  x<<-y
m<<-NULL
}
  get<-function()x   # I get the function
  setsolve<-function(solve) m <<- solve  # I set the command of the function, here solve, to receive the inverse
  getsolve<-function()m # it solves the function
  
  list(set=set, get=get,# to make my values visible, I put them in a list, which happens in my last step.
       setsolve=setsolve,
       getsolve=getsolve) 
  
#####################################################################################
    
cacheSolve <- function(x) { 
  # returns the inverse of matrix x which was calculated in the above step
  
    m <- x$getsolve()  # 'check' the value (inverse of x) which was alreday calculated
    
    if(!is.null(m)) { # if m is available then show the cached value
      return(m)
    }
    
    # but if the cache is not available, 
    data <- x$get()       # receive the value 
    m <- solve(data) # calculate the inverse
    setsolve(m)	      
    return(m)		      #' return it' print' it
  }
  