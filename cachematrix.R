# The two functions below can be used to cache the inverse of a matrix
# and save time when the inverse is needed repeatedly

# The first function makeCacheMatrix makes a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse ot the matrix
# 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  set <- function(y){
    x <<- y
    invr <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invr <<- inverse
  getinverse <- function() invr
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


# The following function calculates the inverse of the special "matrix" 
# created with the above function.
# It first checks, if the inverse has already been calculated. It that's the 
# case it gets the inverse from the cache and skips computation. Otherwise it 
# will calculate the inverse and sets it through the setinverse function.

cacheSolve <- function(x, ...) {
        invr <- x$getinverse()
        if(!is.null(invr)){
          message("getting cached data")
          return(invr)
        }
        data <- x$get()
        invr <- solve(data)
        x$setinverse(invr)
        invr
}
