## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
m<- NULL

setMatrix <- function(nv) 
        {
                x <<- nv
                m <<- NULL
        }
        
        getMatrix <- function() 
        {
                x
        }
        cacheInverse <- function(solve)
        {
                m <<- solve
                
        }
        getInverse <- function() 
        {
                m
        }
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(y, ...)
{
        inverse <- y$getInverse()
        if(!is.null(inverse)) 
        {
        message("getting cached data")
        return(inverse)
        }
        
        data <- y$getMatrix()
        inverse <- solve(data)
        y$cacheInverse(inverse)
        inverse
      
}
