## Methods used for homework 3 of the R programming introduction on Coursera

## Caches the methods required to take the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) 
{
        m <- NULL
        set <- function(y) 
        {
                x <<- y
                m <<- NULL
        }

        get <- function() x

        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m

        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Solves for the inverse of a matrix, using the cached value if available
cacheSolve <- function(x, ...)
{
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if (!is.null(m))
        {
                message("Getting cached data")
                return(m)
        }

        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
