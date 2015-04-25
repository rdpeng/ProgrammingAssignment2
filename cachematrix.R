makeCacheMatrix <- function(x = matrix()) {
        # Controles de la matriz     
        # Control matriz númerica
        if(!is.numeric(x)){
                stop("Matriz no númerica")
        }
        # Control matriz cuadrada
        if (nrow(x) != ncol(x)){
                stop("Matriz no cuadrada")
        }
        # Control matriz con inversa
        if(det(x) == 0) {
                stop("Matriz con determinante 0")
        }
        inv <- NULL
        set <- function(y) {
                x    <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function()      inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
}

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        data <- x$get()
        inv  <- solve(data, ...)
        x$setinv(inv)
        inv
}
