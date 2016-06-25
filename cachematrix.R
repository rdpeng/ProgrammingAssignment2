## The defined functions yield a method for finding the invesre to a
## invertibel square matrix, where the result is read from cache in
## case that it was already calculted before.
## For a matrix X this is done via cacheSolve(makeCacheMatrix(X))


## makeCacheMatrix takes a solvable square matrix x as an argument
## and returns a list which serves as input for cacheSolve(). 

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() {
    x
  }
  setsolve <- function(solve){
    s <<- solve
  }
  getsolve <- function(){
    s
  }
  list(set = set,
       get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve takes as input a list which is returned by
## makeCacheMatrix, e.g. makeCacheMatrix(x), and returns 
## the inverse of x. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
