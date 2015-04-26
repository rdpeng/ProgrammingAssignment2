makeCacheMatrix <- function(matriz = matrix()) {
    inverso <- NULL
    set <- function(x) {
        matriz <<- x;
        inverso <<- NULL;
    }
    get <- function() matriz;
    setinv <- function(inv) inverso <<- inv;
    getinv <- function() inverso;
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}
cacheSolve <- function(matriz, ...) {
    inverso <- matriz$getinv()
    if(!is.null(inverso)) {
        message("Getting cached data...")
        return(inverso)
    }
    data <- matriz$get()
    inverso <- solve(data, ...)
    matriz$setinv(inverso)
    inverso
}
