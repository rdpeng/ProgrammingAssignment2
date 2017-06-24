## Put comments here that give an overall description of what your
## functions do
#makeCacheMatrix generates a 'matrix' object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set=set, get=get,
             setinv=setinv,
             getinv=getinv)
}


#cacheSolve: This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix. If the inverse has already been 
#calculated (and the matrix has not changed), then the cachesolve 
#should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
}
data <- x$get()
inv <- solve(data, ...)
x$setinv(inv)
inv        
}

