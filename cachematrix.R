## the aim of this assignment is to understand Lexical Scoping where you can define varibles in differnat enviroment and call it from different function. The makeCacheMatrix aims to create a matrix and cacuate its inverse and when cacheSolve called it supoose to check if the invers had been caucated by makeCacheMatrix, the cashSolve will print out the inverse otherwise it will be calculated and use setinverse function to store the result in the inverse varble defined in makeCacheMatrix.    

makeCacheMatrix <- function(x = matrix()) {
  inv <- matrix()
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## this function takes an argment as function that holds matrix and functions to get and set the inverse. The cacheSolve will check if the inverse had been calculated or it will be calculated by getting the matrix and use solve function to get the invers which will be pased as an argument to set the inverse varible. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <-matrix() 
  inv <- x$getInverse
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  return(inv)
}


