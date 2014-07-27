## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(xx = matrix()) {
        mm <- NULL
        setMatrix <- function(y) {
                xx <<- y
                mm <<- NULL
        }
        getMatrix <- function() xx
        setMatrixInverse <- function(inverse) mm <<- inverse
        getMatrixInverse <- function() mm
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setMatrixInverse = setMatrixInverse,
             getMatrixInverse = getMatrixInverse)
        

}


## Write a short comment describing this function

cacheSolve <- function(xx, ...) {
       mm <- xx$getMatrixInverse()
        if(!is.null(mm)) {
                #message("getting cached data")
                return(mm)
        }
        data <- xx$getMatrix()
        mm <- solve(data, ...)
        xx$setMatrixInverse(mm)
        mm
        ## Return a matrix that is the inverse of 'x'
}
