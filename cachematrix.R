## Author Jeff Behrens  jeffb@post.harvard.edu

## Syntax to test:
## cacheSolve(makeCacheMatrix(m))
## Where m is a matrix, assume it is invertible (i.e. not singular)

## create a function/vector that defines 4 functions:
##      set
##      get
##      setsolve
##      getsolve

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## function to solve a matrix, but check first if already solved (i.e. m not null)
## stores result in a cached form, global (parent?) environment, in variable m

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    
    ## put some error checking in anyways
    if (det(data)==0) {
        print("Sorry, this matrix has a determinant of 0")
        return()
    }
    m <- solve(data)
    x$setsolve(m)
    m
}