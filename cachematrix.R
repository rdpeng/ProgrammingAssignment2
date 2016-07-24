## Put comments here that give an overall description of what your
## functions do
## Matrix inversion takes up a lot of computing. Hence, it may be a good idea to cache the inverse of a matrix rather than compute it over and over again.
## The next two functions can be used to create a special object that stores a matrix and caches its inverse.

## Write a short comment describing this function
## The following function, assigned to the makeCacheMatrix variable, creates a special "matrix" object which can cache its inverse: 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
}
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## The next function calculates the inverse of the special "matrix" created with the previous function. 
## In doing so, it first checks whether the inverse has already been calculated.
## If this is the case, it retrieves the inverse from the cache and skips the computation. 
## If not, it computes the inverse and puts the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}






