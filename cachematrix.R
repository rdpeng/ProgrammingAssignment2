## makeCacheMatrix function generates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        iv <- NULL
        set <- function(y) {
                x <<- y
                iv <<- NULL
        }
        get <- function() x
        Iset <- function(inverse) iv <<- inverse
        Iget <- function() iv
        list(set = set,
             get = get,
             Iset = Iset,
             Iget = Iget)
}


## cacheSolve return the inverse of the matrix generated using makeCacheMatrix
## It first checks if the inverse is cached 
## If yes - it returns the cached inverse, if no - it calculates and returns the inverse


cacheSolve <- function(x, ...) {
        iv <- x$Iget()
        if (!is.null(iv)) {
               return(iv)
        }
        mx <- x$get()
        iv <- solve(mx, ...)
        x$Iset(iv)
        iv
}
