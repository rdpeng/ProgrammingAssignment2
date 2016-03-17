## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL
  set <- function(y)
  {
    x <<- y
    invmatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inv_matrix)  invmatrix <<- inv_matrix
  getinverse <- function() invmatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x = makeCacheMatrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  invmatrix <- x$getinverse()
  if(!is.null(invmatrix)){
    print("Returning Cached Results")
    return(invmatrix)
  }
  data <- x$get()
  invmatrix <- solve(data)
  x$setinverse(invmatrix)
  invmatrix
  
  
}

