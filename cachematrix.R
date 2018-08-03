##makeCacheMatrix creates a special "matrix" object which
##can cache its inverse
##
##cacheSolve returns the inverse of the matrix object
##created by makeCacheMatrix

##makeCacheMatrix creates a list of four functions
##which are used to get and set the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    set <- function(y){
        x <<- y
        x_inv <<- NULL
    }
    
    get <- function() x
    setInv <- function(inverse) x_inv <<- inverse
    getInv <- function() x_inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


##cacheSolve returns the inverse of the matrix returned by
##makeCacheMatrix.  It checks that matrix hasn't changed and
##that the inverse has not been cached already.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    x_inv <- x$getInv()
    x_mat <- x$get()
    
    #checks that inverse exists and that matrix hasn't changed
    if(!is.null(x_inv) && x_mat == x) {
      message("getting cached inverse")
      return(x_inv)
    }
    #otherwise, finds inverse
    x_inv <- solve(x_mat, ...)
    x$setInv(x_inv)
    x_inv
  
}
