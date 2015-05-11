## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#cache the matrix and the sovled matrix
makeCacheMatrix <- function(x = matrix()) {
  s<- NULL
  #return the matrix
  set<- function(y){
    x<<- y
    s<<- NULL
  }
  get<- function() x
  setsolve<- function(solve) s<<- solve
  #return the solved matrix
  getsolve<- function() s

  list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## Write a short comment describing this function

#calculate the inverse matrix
cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  s<- x$getsolve()
  #if there is a cached result, return directly
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  #if no result cached, solve the matrix and cached
  data<- x$get()
  s<- solve(data)
  x$setsolve(s)
  #return the solved matrix
  s
}

##test the function
#generate a matrix
mx<-matrix(rnorm(16),4,4)
#cache the matrix
x<-makeCacheMatrix(mx)
#calculate the inverse matrix
cacheSolve(x)
#get the result from cache
cacheSolve(x)
