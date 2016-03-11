## These functions place the inverse of a matrix in
## cache or solve for the inverse if it is not present
## in cache

## This function places the inverse of the matrix in the cache

makecachematrix <- function(x = matrix())
{
        inv <- NULL
        set <- function(y)
        {
                x<<- y
                inv<<- NULL
        }
        get <- function()x
        
        setinv <- function(inverse) inv <<- inverse
        
        getinv <- function() inv
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function solves for the inverse of a matrix if it is not
## already in cache

cachesolve <- function(x, ...)
{
        inv <- x$getinv()
        if(!is.null(inv))
        {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}

## By Brian
