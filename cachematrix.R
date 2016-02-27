## I'm creating a matrix function to cache the inverse of a matrix instead of computing it repeatedly
## I have two functions, makeCacheMatrix and cacheSolve, to cache the inverse of a matric

## The below function basically sets the value of the matrix, then gets the value of the matrix.
## Once this is done, it sets the inverse of the matrix, then gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
     x <<- y
     inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function returns the inverse of the matrix after checking if it has been computed. If it hasn't been computed,
## it computes the inverse and stores the value in the cache wiht the help of the setinverse function

cacheSolve <- function(x, ...) {
inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
