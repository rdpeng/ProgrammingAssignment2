## Function returns the inverse of a matrix
## if inverse already calculated, it will retrieve the inverse

## 

makeCacheMatrix <- function(x = matrix()) {
        inv<-Null
        set <- function(y) {
        x <<- y
        inv <<- NULL
     } #not inverse
    getf <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set,
         getf = getf,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Get inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$getf()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}
