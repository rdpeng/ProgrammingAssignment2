## Put comments here that give an overall description of what your
## functions do

## This function creates a special vector capable of caching its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set =set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
        

}


## The function computes the inverse of a vector

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <-x$get()
        i <- solve(data, ...)
        i$setinverse(i)
        i
}
