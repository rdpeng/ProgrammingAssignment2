## Functions to cache the inverse of a matrix

## Creating matrix object that can cache its object
 makeCacheMatrix <- function( M = matrix() ) 
{
  
## Initialization 
  i <- NULL
  
## Set the matrix
  set <- function( matrix )
  {
    M <<- matrix
    i <<- NULL
  }
  
## Get Matrix
  get <- function() 
  {
## Return Matrix
    M
  }
  
## To set the inverse of the matrix
  set_Inverse <- function(inverse) 
  {
    i <<- inverse
  }
  
## Get the inverse of the matrix
  get_Inverse <- function()
  {
## Return the inverse property
    i
  }
  
## Return a list of the methods
  list(set = set, get = get,
       set_Inverse = set_Inverse,
       get_Inverse = get_Inverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

 cacheSolve <- function(x, ...) {
  
## Return a matrix that is the inverse of 'x'
  m <- x$get_Inverse()
  
## Just return the inverse if its already set
  if( !is.null(M) ) {
    message("Getting cached data")
    return(M)
  }
  
## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$set_Inverse(M)
  
  ## Return the matrix
  M
}