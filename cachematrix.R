## Matrix inversion is usually a costly computation.
## There may be some benefit to caching the inverse of
## a matrix rather than compute it repeatedly.

## makeCacheMatrix is used to create and return a list of functions
## used by cacheSolve to get or set the inverted matrix in cache
makeCacheMatrix <- function( m = matrix() ) {
        ## Initialize the inverse property
        ## will set initial function to logical NULL
        i <- NULL
        ## This will set the value of the matrix
        set <- function( matrix ) {
                m <<- matrix   #cache the inputted matrix so that caheSolve
                #can see if it has changed
                i <<- NULL
        }
        ## To get the value of the matrix
        get <- function() {
                ## Return the matrix
                m
        }
        ## To set the inverse of the matrix
        setInverse <- function(inverse) {
                i <<- inverse
        }
        ## Method to get the inverse of the matrix
        getInverse <- function() {
                ## Return the inverse property
                i
        }
        ## Returns a list of the methods
        ##creates a list to house the four functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Computes the inverse of the matrix returned
## by makeCacheMatrix(), however, if the inverse has
## already been calculated, then it retrieves it from the cache.
cacheSolve <- function(x, ...) {
        ## Wil return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        ## Will return the inverse if its already set
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        ## Get the matrix from our object
        data <- x$get()
        ## This will find the inverse using matrix multiplication
        m <- solve(data) %*% data
        ## Set the inverse to the object
        x$setInverse(m)
        ## Return the matrix
        m
}

#test
bmatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
bmatrix$get() 
cacheSolve(bmatrix)
bmatrix$getInverse
bmatrix$getInverse()
bmatrix$getInverse()

