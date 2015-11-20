## simple test matrix to use while testing
a<-1:2
b<-3:4
c<-rbind(a,b)  
d<- solve(c)

## makeCacheMatrix function description
## Creates the get and set functions that will be part of the global environment in the "test" list
## functions will be called from another function to get or set values

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    print(environment())
    evn <- environment()
    print(parent.env(evn))
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    getevn <- function() environment()
    test <<- list(set = set, get = get, setinv = setinv, getinv = getinv, getevn <- getevn)
}


## The cacheSolve function is used to get values from cache
## if the value returned is null, then the inverse of a matrix will be calculated and stored (set) in cache
## if the value returned is not null, then the inverse of the matrix will pulled (get) from cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x2 <- test$get()
    inv2 <- test$getinv()
    message("x2=")
    print(x2)
    message("x=")
    print(x)
    message("inv2=")
    print(inv2)
    if (!is.null(inv2)) {
        message ("getting inverse from cache")
        inv2
    }
    else {
        inv2 <- solve(x)
        message ("calc inverse of matrix and cache it")
        test$setinv(inv2)
    }
}
