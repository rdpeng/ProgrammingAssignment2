## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        a <- NULL
        set <- function(y) {
                x <<- y
                a <<- NULL
        }
        get <- function() x
        setInverso <- function(inverso) a <<- inverso
        getInverso <- function() a
        list(set = set, get = get, 
             setInverso = setInverso,
             getInverso = getInverso)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                a <- x$getInverso()
        if(!is.null(a)) {
                message("getting cached data")
                return(a)
        }
        datos <- x$get()
        a <- Solve(datos, ...)
        x$setInverso(a)
        a
}
