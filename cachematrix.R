##This pair of functions work to cache the inverse of a matrix in order 
##avoid the costly computation of computing the inverse repeatedly


## This first function creates a special "matrix" object that can 
##cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(inv) i <<- inv
      getinverse <- function() i
      list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


##This function returns a matrix that is the inverse of the special "matrix" 
##returned by the makeCacheMatrix function above. If the inverse has 
##already been calculated (and the matrix has not changed), then this function will
##retrieve the inverse from the cache.

cacheSolve <- function(aMatrix, ...) {
        ## Return a matrix that is the inverse of 'x'
      i.local <- aMatrix$getinverse()
      if(!is.null(i.local)) {
            message("getting cached data")
            return(i.local)
      }
      data <- aMatrix$get()
      i.local.calculated <- solve(data, ...)
      aMatrix$setinverse(i.local.calculated)
      i.local.calculated #returns the inverse value
}

##Can test this function with aMatrix <- makeCacheMatrix(matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2))
##Run cacheSolve(aMatrix) two times. The inverse of aMatrix should 
##be matrix(c(6,2,8,4), nrow = 2, ncol = 2) and the message "getting cached data" will appear"