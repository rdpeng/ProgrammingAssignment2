library(matrixcalc)

makeCacheMatrix <-  function(X = matrix()) {
  # This function creates a special "matrix" object that can cache its inverse.
  invX <- NULL
  set <- function(y) {
    X <<- Y
    invX <<- NULL
  }
  get <- function() X
  setInv <- function(solve) invX <<- solve
  getInv <- function() invX
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
}

cacheSolve <- function(X,...) {
  # This function computes the inverse of the special "matrix" returned 
  # by makeCacheMatrix above. If the inverse has already been calculated 
  # (and the matrix has not changed), then the cachesolve should retrieve 
  # the inverse from the cache.
  invX <- X$getInv()
  if(!is.null(invX)) {
    message("getting cached data")
    return(invX)
  }
  data <- X$get()
  data
  if (is.singular.matrix(data) == FALSE) {
    invX <- solve(data, ...)
    X$setInv(invX)
    invX
  } else {
    "Matrix is singular, try again"

  }

}

#Initiate invertible matrix
A <- matrix(c(5, 1, 0,
              3,-1, 2,
              4, 0,-1), 
          nrow=3, byrow=TRUE)

#Populate a new variable holding both original matrix and an inverted version
invTest <- makeCacheMatrix(A)
# Print original matrix
invTest$get()

#First run, no cached data available since invX = NULL
cacheSolve(invTest)
#Second run of cahcesolve(), invX populated with the inverse and therefore we grab the cached result
cacheSolve(invTest)


#Test with singular matrix
B <- matrix(rep(1,25),nc=5) 

#Populate a new variable holding both original matrix and an inverted version
invTest <- makeCacheMatrix(B)
# Print original matrix
invTest$get()

#First run, abort message since matrix is singular
cacheSolve(invTest)
