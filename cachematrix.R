##  makeCacheMatrix creates a special vector which is a list containing function to 
##  set the value of the matrix, get the value of the matrix, set the value of the inverse,
## and get the value of the inverse


makeCacheMatrix <- function(x = matrix())  
{
    i <- matrix()
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve function calculates the inverse of the matrix after checking whetherit
## has been calculated earlier in which case it returns cache value. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
  }
