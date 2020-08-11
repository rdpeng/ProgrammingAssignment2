## The 1st function will accept and cache a square, invertible matrix.
## The second function will accept the first function and provide
## the inverse of the given matrix. The inverted matrix will 
## then be cached. the cache feature alleviates additional 
## computational burden associated with the required linear
## algebra necessary to perform the inverse operation.
## My functions relied heavily upon the provided examples.

## This matrix uses a reasonably large matrix to highlight the 
## benefit of caching data for computationally large workloads.
example <- matrix(rnorm(1000000), 1000, 1000)


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get, 
       setSolve = setSolve,
       getSolve = getSolve)
}


## This function will take and store the "example" matrix in memory
## The sub-functions will apply valeues to variables and additional 
## functions in the form of a list.

## This statement creates a variable for the second function
makeCacheMatrix.examplePart2 <- makeCacheMatrix(example)


cacheSolve <- function(x, ...) {
   m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}

## This second function takes a variable that assigns the matrix to the
## first function producing am inverse matrix. If the results are 
## already cached then the cached result will be produced. 


cacheSolve(makeCacheMatrix.examplePart2)
## This statement applies the second function to the variable that
## assigns the initial matrix to the first function.

