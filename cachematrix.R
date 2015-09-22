## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- matrix()
  set <- function(y) {
    x <<- y
    inv <<- matrix()
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
# Check if fist element of matrix is not na then return cashed data
  if(!is.na(inv[1,1])) {
    message("getting cached data")
    return(inv)
  }
  
# If the cashed data is na then calculate the inversion of matrix
  
  data <- x$get()

  #create identity metrix
  n <- nrow(data)
  I <- matrix(rep(NA,n*n), nrow = n, ncol = n)
  for(i in 1:n){
    for(j in 1:n){
      if(i == j) I[i,j] = 1 else I[i,j] = 0
    }
  }
  inv <- solve(data,I)
  x$setinv(inv)
  inv
}