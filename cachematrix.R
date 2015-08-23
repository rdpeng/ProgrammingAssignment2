makeCacheMatrix <- function(x = matrix()) {
    
    #==================================================================
    # This function (makeCacheMatrix) creates a 'special' 
    # vector that contains four functions.
    #       1.  set
    #       2.  get
    #       3.  setinver
    #       4.  getinver
    # These four functions will be used in the cacheSolve() function.
    # the '<<-' operation allows variables to be assigned a values
    # that are outside the scope of the function.
    #====================================================================
    
    inver <- NULL
    set <- function(y) {
        x <<- y
        inver <<- NULL
    }
    get <- function() x
    setinver <- function(inverse) inver <<- inverse
    getinver <- function() inver
    list(set = set, get = get,
         setinver = setinver,
         getinver = getinver)
}

cacheSolve <- function(x, ...) {
    # ==================================================================
    # This function cacheSolve() calculates the inverse of a given matrix.
    # It then stores the inverse of this given matrix in cache.
    # If the inverse of the same matrix is required later then
    # it is not calculated but returned directly from cache 
    # thus saving computing time for especially for large matrices.
    # It can do this for any number of matrices
    # within the limits of the hardware.
    #====================================================================
    inver <- x$getinver()
    if(!is.null(inver)) {
        message("Inverse already calculated. Here it is from cache")
        return(inver);
    }
    data <- x$get()
    inver <- solve(data, ...);
    x$setinver(inver);
    message("Hold on a second. I must calculate the invere now...!")
    inver;
}
