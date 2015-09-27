## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  # Initialized matrix inv as NA for first time use
  inv <- matrix()
  
  set <- function(y) {
    # Store y to (global) matrix x 
    x <<- y
    # reset (global) matrix inv to NA every time new data set
    inv <<- matrix()
  }
  
  get <- function() x
  # Store i to (global) matrix inv
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ## Call for inverse metrix of x & store in matrix inv
  inv <- x$getinv()
  
  # Check if fist element of inv is not na then return cashed data
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
  # Use fn solve for equation of matrix ax = b for matrix a x inv of matrix a result = identity matrix
  # or a (a-1) = i then x = a-1 
  inv <- solve(data,I)
  x$setinv(inv)
  inv
}