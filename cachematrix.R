## these functions work together to cache the inverse of invertable matrices to avoid having
## having to perform redundant calculations on data that hasn't changed
## example:
## m <- matrix(c(1,3,5,7,9,2,4,6,8),3,3)
## mCache <- makeCacheMatrix(m)
## cacheSolve(mCache) - returns inverse
## cacheSolve(mCache) - returns cached inverse

## This function creates a special "matrix" object
## that can cache its inverse.
## It returns a list that contains functions to:
## 1.  set the value of the matrix - set()
## 2.  get the value of the matrix - get()
## 3.  set the inversion of the matrix - setinverse()
## 4.  get the inversion of the matrix - getinverse()

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached inverse matrix")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
