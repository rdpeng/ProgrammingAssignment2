## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## create a Cache matrix function with a call to list function

makeCacheMatrix <- function(x = matrix()) {
inv = NULL
  set = function (y){
  ## assigning variables in different environment
    x <<-Y
    inv <<-NULL
  }
 get = function() x
 setinv = function(inverse) inv <<- inverse
 getinv = function () inv
 list(set=set, get=get, setinv=setinv, getinv=getinv)
  

}


## Write a short comment describing this function
## this function is used to return the value of  inverse matrix 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv = x$getinv()
  if (!is.null(inv))
    {
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv) 
  }

mat.data = x$get()
inv = solve(mat.data, ...)
x$setinv(inv)

return(inv)
}
