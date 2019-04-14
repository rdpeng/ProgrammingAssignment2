## This function creates a special "matrix" object that can cache its inverse.
makecatchematrix <- function(x = matrix()) {
        n <- NULL
        set <- function(z) {
                x <<- z
                n <<- NULL
        }
        get <- function() x
        setin <- function(inverse) n <<- inverse
        getin <- function() n
        list(set = set, get = get,
             setin = setin,
             getin = getin)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
##Assumption is that matrix in above is invertible

catchesolve <- function(x, ...) {
               n <- x$getin()
        if(!is.null(n)) {
                message("getting cached data")
                return(n)
        }
        data <- x$get()
        n <- solve(data, ...)
        x$setin(n)
        n
}

