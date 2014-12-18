## The first function, `makeCacheMatrix` creates a special "matrix", which 
## contains a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the solved matrix
## 4.  get the value of the solved matrix

makeCacheMatrix <- function(x = matrix()) {
    solved <- NULL
    
    ## implement the first feature
    set <- function(y){
        x <<- y
        solved <<- NULL
    }
    
    ## implement the second feature
    get <- function() x
    
    ## implement the third feature
    setSolved <- function(s) solved <<- s
    
    ## implement the fourth feature
    getSolved <- function() solved
    
    list(set = set, get = get, setSolved = setSolved, getSolved = getSolved)
    
}


## The following function calculates the solve of the special "matrix"
## created with the above function. However, it first checks to see if the
## solve has already been calculated. If so, it `get`s the solve from the
## cache and skips the computation. Otherwise, it calculates the solve of
## the data and sets the value of the solve in the cache via the `setSolved`
## function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getSolved()
    if(!is.null(s)){
        message("getting cached data")
        return(s)
    }
    
    data <- x$get()
    s <- solve(data)
    x$setSolved(s)
    s
}
