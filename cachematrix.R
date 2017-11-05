## These functions create a cache for matricies, and then a function to calculate the inverse and store it.

## Create the matrix

makeCacheMatrix <- function(mat = matrix()) {
      matinv <- NULL
      set <- function(y){
            mat <<- y
            matinv <<- NULL
      }
      get <- function() mat
      setinverse <- function(inverse) matinv <<- inverse
      getinverse <- function() matinv
      list(set = set,
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## Calculate the inverse of the matrix, first retrieving from cache if available

cacheSolve <- function(m, ...) {
      ## Return a matrix that is the inverse of 'x'
      matinv <- m$getinverse()
      if(!is.null(matinv)){
            message("getting cached data")
            return(matinv)
      }
      mat <- m$get()
      matinv <- solve(mat,...)
      m$setinverse(matinv)
      matinv
}