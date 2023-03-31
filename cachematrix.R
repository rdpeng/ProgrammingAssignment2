#
# Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
makeCacheMatrix <- function(x = matrix() ){
  s<- NULL
#initialisation of objects
  set<- function(y){
    x<<- y
  #Passes all arguments of set to x in parent enviroment
    s<<- NULL
  #Clears any value of m that had been cached by a prior execution of function solve
  }

  get<- function()x
  setsolve<- function(solve) s<<- solve
  #Passes result from solve to s in parent enviroment
  getsolve<- function()s
  # Looks up for s to find its value in solve
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}
# In this list we assign names to previously created functions and


cacheSolve<- function(x, ...){
#retrieve solve from the object passed as argument
  s<- x$getsolve()
  # we have created list with function names which allows us to use operator $ in order to get solve from cache
  if (!is.null(s)){
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  # defines part of data you want to inverse
  s<- solve(data, ...)
  x$setmean(s)
  # s to caching
  s
}
