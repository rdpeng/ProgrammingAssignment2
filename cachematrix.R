## assignment 2, week 3
## Neil Sethi
## cachematrix.R
## Calculate and cache the inverse of a matrix

## Make a CacheMatrix ('matrix') with four function 'properties': set, get, setinv, getinv
makeCacheMatrix <- function(x = matrix()) 
{
        # initialize result to an empty matrix
        # xinv <-matrix(,nrow(x), ncol(x))
        xinv <- NULL
        
        # Assume x is invertible
        
        set <- function(mtx)
        {
                x <<- mtx
                xinv <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(mtx) xinv <<- mtx
        
        getinv <- function() xinv
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Get the inverse of CacheMatrix x
##      CacheMatrix x is returned from calling makeCacheMatrix
cacheSolve <- function(x, ...) 
{        
        ## Return a matrix that is the inverse of 'x'
        xinv <- x$getinv()
        
        if (!is.null(xinv))
        {
                # Inverse alreay calculated
                # Check if matrix x has not changed and return the cached inverse
                print ("Getting cached matrix inverse")
                return (xinv)
        }
        data <- x$get()
        xinv <- solve (data)
        
        x$setinv(xinv)
        
        xinv
}