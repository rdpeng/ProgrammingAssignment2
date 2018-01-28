## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix creates a list containing a function to
# 1. set  matrix
# 2. get matrix
# 3. set matrix
# 4. get matrix
makeCacheMatrix <- function(x = matrix()) 
    {
         inv <- NULL
         set <- function(y)
         # '<<-' assigns a value to an object in an environment
         # different from the current one.
         {
         x <<- y
         inv <<- NULL
         }
         
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set,
          get=get, 
          setinverse=setinverse,  
          getinverse=getinverse)
}

#x- outout of makeCacheMatrix()

cacheSolve <- function(x, ...) 
{
    inv <- x$getinverse()
    #if teh inverse has already been calculated
    
    if(!is.null(inv)) 
    #get it from the cache and skips teh computation
    
    {
        message("getting cached data.")
        return(inv)
    }
    # otherwise, calculates teh inverse
    
    data <- x$get()
    inv <- solve(data)
    #sets the value of the inverse in the cache trough the setinverse function
    x$setinverse(inv)
    inv
}
