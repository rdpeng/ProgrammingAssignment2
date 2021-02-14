## makeCacheMatrix is used to create a matrix for caching its inverse.
## You have to assign this function to a value before call it.
## For example:
## >pmatrix<-makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
## >pmatrix$get()
makeCacheMatrix<-function(x = matrix()){
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function() {x}
      setInverse <- function(inverse){inv <<- inverse}
      getInverse <- function() {inv}
      list(set = set, get = get, 
           setInverse = setInverse, 
           getInverse = getInverse)
}

## cacheSolve is used to compute the inverse of the matrix that is created by makeCacheMatrix.
## You can call this function by using the value that you've just assigned to makeCacheMatrix
## (in this case, pmatrix) to get the inverse of the matrix.
## for example:
## cacheSolve(pmatrix)
cacheSolve <- function(x, ...){
      inv <- x$getInverse()
      if(!is.null(inv)){
              message("getting cached data")
              return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setInverse(inv)
      inv
}
