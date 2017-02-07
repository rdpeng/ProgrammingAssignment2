## Here a pair of functions that cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(myMat = matrix()) {
      InvMat <- NULL
      set <- function(y) {
            m <<- y
            InvMat <<- NULL
      }
      
      get <- function() myMat
      setInv <- function(invertedM) m <<- invertedM
      getInv <- function() InvMat
      list(set = set, get = get,
           setInv = setInv,
           getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(myMat, ...) {
      InvMat <- myMat$getInv()
      if(!is.null(InvMat)) {
            message("getting cached data")
            return(m)
      }
      data <- myMat$get()
      InvMat <- solve(data, ...)
      myMat$setInv(InvMat)
      InvMat
        ## Return a matrix that is the inverse of 'x'
}