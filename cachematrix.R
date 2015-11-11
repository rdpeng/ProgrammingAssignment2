## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
   minv <- NULL                 
       
    set <- function(m) {
      if (!identical(x,m)) {
            x <<- m
            minv <<- NULL }  
        #to satisfy condition "and the matrix has not changed"
        #we reset x to m and assign minv as NULL only in case if new matrix m is different
        #with already cached one
      }
    get <- function() x
    setinv <- function(minverse) minv <<- minverse
    getinv <- function() minv     
 
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    minv <- x$getinv()

    if(!is.null(minv)) {
      message("Inverse matrix is already calculated. Getting data from cache")
      return(minv)
    }

    data <- x$get()          
    minv <- solve(data, ...)  
    x$setinv(minv)            
    minv
}
