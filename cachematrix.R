## Put comments here that give an overall description of what your
## functions do
## It crates a special "matrix" object ,which can cache its inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
i  <- NULL
        set  <- function(y){
                x <<- y
                i <<- NULL 
        }
        get  <- function() x
        setinverse  <- function(inverse) i  <<- inverse
        getinverse  <- function() i
        list(set= set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## Write a short comment describing this function
## finds the inverse. 
## cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {

        
          i  <- x$getinverse()
        if (!is.null(i)){
                message("Retriving cached content")
                return(i)
        }
        data  <- x$get()
        i  <- solve(data, ...)
        x$setinverse(i)
        i
        
        
}
