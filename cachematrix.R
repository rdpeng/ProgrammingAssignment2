## these two functions create a matrix and then calculate the inverse of that matrix
## returning the inverse from cache if it already exists there


makeCacheMatrix <- function(x = matrix()) 
    
## This function creates a special "matrix" object that can cache its inverse.

{
    # clear out cache
    
    cache <- NULL
    
    # function to store an input matrix and clear out cache
    
    setMatrix <- function(newValue) 
    {
        x <<- newValue
        cache <<- NULL
    }
    
    # function to grab the stored matrix
    
    getMatrix <- function() 
    {
        x
    }
    
    # function to calculate the inverse and store it in cache
    
    cacheInverse <- function(solve) 
    {
        cache <<- solve
    }
    
    # function to grab the cached matrix
    
    getInverse <- function() 
    {
        cache
    }
    
    # return a list. Each named element of the list is a function
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         cacheInverse = cacheInverse,
         getInverse = getInverse)

}


cacheSolve <- function(y=matrix(), ...) 
    
## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.
    
{
    
    # get the cached value

    inverse <- y$getInverse()
    
    

    # if a cached value exists return it
    
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    # otherwise get the matrix, caclulate the inverse and store it in
    # the cache
    
    data <- y$getMatrix()
    inverse <- solve(data)
    y$cacheInverse(inverse)
    
    # return the inverse
    inverse
}



test <- makeCacheMatrix()

test$set(matrix(1:4,2,2))

cacheSolve(test)  # first call calcs inverse
cacheSolve(test)  # subsequent calls pull from cache
cacheSolve(test)  # subsequent calls pull from cache

