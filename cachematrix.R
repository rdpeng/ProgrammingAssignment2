## There are many benefits to caching the inverse of a matrix. 
## One of them is, you don't have to compute it repeatedly.
## These are functions used to create specific objects that store matrices and caches its inverse.

## This function will create a special matix object that will then cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
}
get <- function() x
setInverse <- function(inverse) inv <<- inverse 
getInvese <- function() inv
list(set = set,
     get = get,
     setInverse = setInverse,
     getInverse = getInverse)

## The makeCacheMatrix function above computes the inverse of the special matrix.
## When the inverse gets calculated, it will retrieve the inverse from the cache if the matrix didnt't change.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat,...)
        x$setInverse(inv)
        inv
}
