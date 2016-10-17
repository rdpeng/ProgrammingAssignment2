# Computing the inverse of a square matrix.

#Function creates a special ¡°matrix¡± and provides function to set values,
#get values,set inverse, get inverse

makeCacheMatrix <- function(x = matrix()) 
{
    ret <- NULL
    set <- function(y) 
    {
        x <<- y
        ret <<- NULL
    }
    get <- function() x
    
    setInverse <- function(solve) 
    {
        ret <<- solve
        # store the matrix that resulted in getting inverse
        # so we can compare next time for equality if we can reuse old inverse or
        # recalculate
        stored <<- x
    }
    getInverse <- function() ret
    getStored <- function() stored
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse,
         getStored = getStored,
         getStored = getStored)
}


# Below Function will look into cache to see if inverse exist for matrix and 
# retrieve the value.. If the inverse has already been calculated 
# (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x,...) 
{
    # Return a matrix that is the inverse of ¡®x¡¯
    ret <- x$getInverse()
    
    if(!is.null(ret))
    {
        message("getting cached data")
        return(ret)
    }
    
    data <- x$get()
    ret <- solve(data)
    x$setInverse(ret)
    ret
}
