## this programme evaluate the inverse of a matrix and also store it in cache which can be access very fast if required in future.

## this function is made to access the value of matrix and its inverse. If the inverse of the matrix is already in cache it will return soon otherwise cacheSolve is used to set and return the inverse of matrix , which can be access in future.

makeCacheMatrix <- function(x = matrix()) {
        m<- NULL
        set<- function(y){
                x<<- y
                m<<- NULL
        }
        get<- function() x
        setSolve<- function(solve) m<<- solve
        getSolve<- function() m
        list(get= get, set= set, getSolve= getSolve, setSolve= setSolve)
}


## It helps in returning the inverse of matrix, but if there is no cache of inverse it will set it and then print it

cacheSolve <- function(x, ...) {
        m<- x$getSolve()
        if(!is.null(m)){
                message("printing cache value")
                m
        }
        data<- x$get()
        m<- solve(data)
        x$setSolve(m)
        m
}
