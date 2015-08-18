## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix())
{
 inv <- NULL# Set initial solution to NULL
 set <- function(y) #then x needs to be changed to the passed in matrix and inv needs to be reset to NULL
 {
  x <<- y
  inv <<- NULL
  }
 get <- function() x # get will returned the stored matrix
 setinverse <- function(inverse) inv <<- inverse #setinverse will allow for storing the solution
 getinverse <- function() inv # getinverse returns the stored value of inv
 list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}
  
 
## Write a short comment describing this function
 
cacheSolve <- function(x, ...) 
{  # attempt to get inverse of the matrix 
 inv <- x$getinverse()  # get  inversed matrix from object x
 if(!is.null(inv)) # if there is a value for m cache-ed, a message will return along with m.
 {
  message("getting cached data.")
  return(inv)
  }
 data <- x$get() # inverse will be calculated.
 inv <- solve(data)
 x$setinverse(inv) #  result will  stored into x's Cache.
 inv # Return a matrix that is  inverse of 'x'
}
