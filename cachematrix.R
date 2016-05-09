## makeCacheMatrix()    - Create an iversed matrix of the given matrix
## cacheSolve()         - inverses the matrix using makeCacheMatrix() OR
##                      - If the inversed matrix is alreay in the cache, it returns the cached value

## This function creates(returns) 4 functions to recieve a matrix and inverse it.
makeCacheMatrix <- function(x = matrix()) {
     
     inversedMatrix <- NULL
     
     setVal <- function(y){
          x <<- y
          inversedMatrix <<- NULL
     }
     
     getVal <- function() x
     setInv <- function(z) inversedMatrix <<- z
     getInv <- function() inversedMatrix
     list(setVal = setVal, getVal = getVal,
          setInv = setInv,
          getInv = getInv)
}

## This function checks if the inverse in already cached. If not, this function inverses the matrix using makeCacheMatrix().
## If the inversed matrix is alreay in the cache, it returns the cached value.
cacheSolve <- function(x, ...) {

     inversedMatrix <- x$getInv()               ## check if already cached
     if(!is.null(inversedMatrix)) {
          message("getting cached data")
          return(inversedMatrix)
     } else {                                   ## calculate if not cached
          data <- x$getVal()
          inversedMatrix <- solve(data, ...)
          x$setInv(inversedMatrix)
          inversedMatrix                        ## Return a matrix that is the inverse of 'x'
     }
}
