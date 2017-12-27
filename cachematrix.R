## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function would create and Cache the matrix inverse result and provide 
#functionality to access those results
makeCacheMatrix <- function(x = matrix())
{
    #initialize the result to NULL for first use
    inverseOfMatrix <- NULL
    set <- function(y)
    {
        x <<- y
        inverseOfMatrix <<- NULL
    }
    
    #methods to access inverse results and set inverse results
    get <- function() x
    setInverse <- function(solve) inverseOfMatrix <<- solve
    getInverse <- function() inverseOfMatrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function
#This function would use Caching to provide results for inverse operation
cacheSolve <- function(x, ...)
{
    ## Return a matrix that is the inverse of 'x'
    #Get Inverse result
    inverseResult <- x$getInverse()
    
    #Check if result/inverse exists ot not
    if(!is.null(inverseResult))
    {
        message("getting cached result for matrix inverse")
        return(inverseResult)
    }
    
    #If above check fails, create a chache of the result/inverse after calculation
    data <- x$get()
    inverseResult <- solve(data, ...)
    x$setInverse(inverseResult)
    inverseResult
}
