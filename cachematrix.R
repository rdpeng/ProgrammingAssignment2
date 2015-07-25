### Matrix inversion is usually a costly computation and
### there may be some benefit to cache the inverse of a matrix rather than compute it repeatedly.

## The following functions work together to create a square matrix
## and make the inverse of the matrix available in the cache environment.

## makeCacheMatrix creates a list containing functions to
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of inverse of the matrix
##  4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix())
{
   inv <- NULL

   # set the value of the matrix
   set <- function(y)
   {
      x <<- y
      inv <<- NULL
   }

   # get the value of the matrix
   get <- function() x

   # set the value of inverse of the matrix
   setinverse <- function(inverse) inv <<- inverse

   # get the value of inverse of the matrix
   getinverse <- function() inv

   # return the created functions to the working environment
   list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
} 


## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix.
## It first checks if the inverse has already been computed. If so, it gets the result and skips the computation.
## If not, it computes the inverse, sets the value in the cache via setinverse function.
## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...)
{
   # get the inverse of the matrix stored in cache
   inv <- x$getinverse()

   # if the inverse has already been calculated
   if(!is.null(inv))
   {
      # get it from the cache and skips the computation
      message("getting cached data.")
      return(inv)
   }

   # otherwise, calculates the inverse 
   data <- x$get()
   inv <- solve(data)

   # set the value of the inverse in the cache via the setinverse function
   x$setinverse(inv)

   # display matrix in console
   return(inv)
}
