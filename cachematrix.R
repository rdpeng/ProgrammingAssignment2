## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## makeMatrix: first initilize x with a matrix and 's' with NULL. ('s' will be the inverse of the square matrix). 
## Define a 'set'-function that shall take a square matrix as input. Whenever set is called 's' will be set to NULL
## Define a 'get'-function. When called it will return the created matrix.
## 'setsolve' will store the inverse matrix in the cache
## 'getsolve' will call this cached matrix and return it
## list() creates the connection/gives names to the functions so we can all them

makeMatrix <- function(x = matrix(numeric(), i, j)) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() {
                return(x)
        }
        setsolve <- function(solve) {
                s <<- solve
        }
        getsolve <- function() {
                return(s)
        }
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cachesolve takes a created matrix object form makeMatrix, calculates the inverse, returns it
## and stores it in the cache 
## first it calls the current value/matrix stored in 's'
## when it is not NULL the matrix object has not changed and therefore the inverse matrix has not changed
## so the cached matrix 's' will be returned and the user will be notified
## otherwise, so if there is a new matrix object, it will be stored in 'data'
## then the inverse of 'data' will be created and stored in 's'
## finally the inverse matrix 's' will be returned

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        return(s)
}
