## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
         invertedmatrix <- NULL
        setmatrix <- function(y) {
                x <<- y
                invertedmatrix <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(solve) invertedmatrix <<- solve
        getinverse <- function() invertedmatrix
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse, getinverse = getinverse)


}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                invertedmatrix <- x$getinverse()
        if(!is.null(invertedmatrix)) {
                message("getting cached data")
                return(invertedmatrix)
        }
        data <- x$getmatrix()
        invertedmatrix <- solve(data)
        x$setinverse(invertedmatrix)
        invertedmatrix

}
