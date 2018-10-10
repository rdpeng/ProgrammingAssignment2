## Create 2 functions that cache the inverse of a matrix


## 1stfunction: makeVector creates a special "vector"
makeCacheMatrix <- function( m = matrix() ) {

	   i <- NULL

    ## Method to set the value of the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## Get the value of the matrix
    get <- function() {
    	m
    }
     ## set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }


  ## Get the inverse of the matrix
    getInverse <- function() {
             i
    }

    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## 2nd function:  computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

        if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }


    ## Get the matrix 
   data <- x$get()

    ## Calculate the inverse 
    m <- solve(data) %*% data

    ## Set the inverse 
   x$setInverse(m)

     m
}
