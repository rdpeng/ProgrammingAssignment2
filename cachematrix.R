## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function creates a list of four functions which includes a matrices and its inverse, 
#as well as set functions
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) { #setter function
    x <<- y
    i <<- NULL
  }
  get <- function() x #returns the matrix
  setinverse <- function(inverse) i <<- inverse #sets the inverse matrix
  getinverse <- function() i #returns the inverse matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) #returns the list
}


## Write a short comment describing this function
#This function returns the inverse of x, caching it if need be or pulling from cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse() 
  if(!is.null(i)) { #checks if it is in cache
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data) #if not in cache, finds the inverse
  x$setinverse(i)
  i
  
}
