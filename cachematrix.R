## ----------------------------------------------------------------------------
## makeCacheMatrix creates a matrix object that can cache its inverse 
## cacheSolve checks returns the inverse of the matrix
## ----------------------------------------------------------------------------

## ----------------------------------------------------------------------------
##      Function makeCacheMatrix (x = matrix)
##
## This function returns a special list of functions and stores the matrix "x"   
## and its inverse "inv" (once it has been computed) in the function environment 
##
## It first initiates the inverse of the matrix. It then defines the following
## functions:
##      (1) "set" resets the matrix to the matrix supplied in the main argument 
##          and resets the inv (the cached inverse of the matrix) to NULL
##      (2) "get" returns the matrix stored in the environment
##      (3) "setinverse" assigns the value to the variable "inv"
##      (4) "getinverse" returns the value of "inv"
##
## Comments: How can I access the "inv" and "x" variables directly? Try this:
##      > e<-environment(cm$get)
##      > e$x
##      > e$inv
## ----------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function (y) {
        #x <<- y # This is probably redundant?
        inv <<- NULL
    }
    get <- function ()
        x
    setinverse <- function (imatrix)
        inv <<- imatrix
    getinverse <- function ()
        inv
     list(
         set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse
    )
}

## ----------------------------------------------------------------------------
##      Function cacheSolve (x, ...)
## 
## This function returns the inverse of the matrix in the supplied object. It
## assumes that the matrix in the supplied object is indeed invertible (there is
## no check for singularity etc). 
##
## It first checks whether the inverse had allready been calculated and cached
## (via x$getinverse()) in the supplied object x:
##      (1) If yes, it returns the caluclated inverse from the cache. 
##      (2) If not, it retrieves the matrix from the object and computes the 
##          inverse. The computed inverse is then stored in x$. 
## ----------------------------------------------------------------------------

# A function that check whether the inverse of the matrix had allready been
# calculated (and cached)
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
