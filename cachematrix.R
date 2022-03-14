## My fonction will cache an inverse of a matrix. So if i need that particular
## inverse, the code will not need to calculate twice the inverse, but will 
## get the cached inverse.

## This function will contain 4 function, set(matrix), get (matrix),
## setInv and getInv

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function () x
        setInv <- function(SolveMatrix) Inv <<- SolveMatrix
        getInv <- function() Inv
        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function will calculate and contain the inverse of a matrix.
## If i need a inverse of a specific matrix, i will use this function and it
## return the answer.

cacheSolve <- function(x, ...) {
        Inv <- x$getInv()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data,...)
        x$setInv(Inv)
        Inv
}
