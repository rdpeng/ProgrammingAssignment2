## The functions take in a matrix and then provide the inverse.  If the 
## matrix inverse is already calculated and stored in the 1st vector, the 
## 2nd variable retrieves the matrix and returns the value. Otherwise, it 
## calculates the inverse and returns it  

## This function creates a special matrix object that can cache the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
              x <<- y
              m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of a matrix if one is not already stored
## in the function above.  If one is stored, it retrieves and returns the value

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
