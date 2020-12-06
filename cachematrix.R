
makeCacheMatrix <- function( m = matrix() ) {

        a <- NULL
        set <- function(y) {
                m <<- y
                a <<- NULL
        }
        get <- function() m
        setInverse <- function(inverse) a <<- inverse
        getInverse <- function() a
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

cachemean <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data) %*% data
        x$setInverse(m)
        m
}


 
