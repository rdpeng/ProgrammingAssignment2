## Today we will try to create some useful functions that help us cache the inverse of a matrix,
## rather than compute it repeatedly.

## Our first function creates a special "matrix" object that can cache its inverse.
## Our function will be a list of following functions:
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse matrix
#get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
            inv <- NULL
            
            set_mx <- function(y) {
              x <<- y
              inv <<-NULL
            }
            
            get_mx <- function() x
            set_inv <- function(inverse) inv <<- inverse
            get_inv <- function() inv
            list(set_mx = set_mx, get_mx = get_mx, set_inv = set_inv, get_inv = get_inv)
}

## Next function takes the speial "matrix" returned above as an argument.
## First, we check if the inverse has been already calculated (and matrix hasn't changed),
## then our new function (cachesolve) should retrieve this inverse from cache:

cacheSolve <- function(x, ...) {
   inv <- x$get_inv()
   if(!is.null(inv)) {
     message("getting cached data")
   }
          ## Next, we write a code to return a matrix that is the inverse of 'x'
   mx <- x$get_mx()
   inv <- solve(mx, ...)
   x$set_inv(inv)
   inv
}
