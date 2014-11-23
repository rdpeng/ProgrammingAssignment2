makeCacheMatrix <- function(m = matrix() ) {      ## create a function for the matrix, set the matrix and put the inverse property
    i <- NULL
    set <- function(matrix) {
            m <<- matrix
            i <<- NULL
    }
    get <- function() {             ## get the function, set inverse of the matrix and get the inverse of the matrix
  	m
    }
    setInverse <- function(inverse) {
        i <<- inverse
    }
    getInverse <- function() {
        i
    }

    list(set = set, get = get,       ## make a list
         setInverse = setInverse,
         getInverse = getInverse)
}

cacheSolve <- function(x, ...) {      ##use cacheSolve, make a matrix that is inverse 
    m <- x$getInverse()
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }
    data <- x$get()         ## get the data
    m <- solve(data) %*% data
    x$setInverse(m)

    m
}
