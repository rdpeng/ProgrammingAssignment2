## store the matrix and Cache its inverse
##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {  
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

#example working.
> a=makeCacheMatrix()
> a$set(matrix(c(2,4,6,7),2,2))
> a$get()
     [,1] [,2]
[1,]    2    6
[2,]    4    7
> a$getInverse
function() inv
<environment: 0x000001daabeb4a50>
> cacheSolve(a)
     [,1] [,2]
[1,] -0.7  0.6
[2,]  0.4 -0.2
