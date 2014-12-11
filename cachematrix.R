## These functions get inverse of a matrix from cache

## the function makeCacheMatrix creates a matrix consisting of 4 functions
## the 4 function set the matrix, get the matrix, set the inverse of matrix and
# get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
        x <<- y
        Inv <<- NULL
}
get <- function() x
setInverse <- function(Inverse) Inv <<- Inverse
getInverse <- function() Inv
matrix(c(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse,),2,2)
}


## cacheSolve function retrieves the inverse from above function
## if it is already calculated. ELse it calculates inverse using Solve() 

cacheSolve <- function(x, ...) {
        Inv <- x[2,2]()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- x[2,1]()
        Inv <- Solve(data, ...)
        x[1,2](Inv)
        Inv
}
