## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    get <- function() x
    setImatrix <- function(Imatrix) m <<- Imatrix
    getImatrix <- function() m

    # return a list of functions as an R object
    list(get=get, setImatrix=setImatrix, getImatrix=getImatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getImatrix()
    if(!is.null(m)){
        message("Cached data found. Getting result... Done.")
        return(m)
    }
    else {
        message("No cached data found. Calculating inverse matrix...")
        data <- x$get() # obtains matrix from object x
        m <- solve(data) # finds inverse matrix
        x$setImatrix(m) # assigns resulting inverse matrix to object x
        message("Done.")
        return(m)
    }
}
