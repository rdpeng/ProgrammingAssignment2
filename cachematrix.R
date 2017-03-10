## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix and returns a list containing accessor and mutator functions for the matrix
## and its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
    m<-NULL
    set<-function(y)
    {
        # Fill in the values of the matrix and null out the inverse
        x<<-y
        m<<-NULL
    }
  
    get<-function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) 
{
    m <- x$getInverse()
    if(!is.null(m)) 
    {
        print("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
