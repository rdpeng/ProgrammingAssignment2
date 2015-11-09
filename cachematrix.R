## These functions cache inverse of a matrix, to reduce calculation time
##
##   makeCacheMatrix() -> this function creates variables which will be cached 

makeCacheMatrix <- function(x = matrix()) {
        matrix <- NULL                  
        set    <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(matinv) matrix <<- matinv
        getinv <- function() matrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


##   cacheSolve()      -> checks cache first for values if not found calls the solve() for calculating inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        matrix <- x$getinv()   # call getinv() to get matrix variable stored in CACHE
        if(!is.null(matrix)) { # do we have a matrix existing in CACHE
                message("getting cached data")
                return(matrix)
        }
        data <- x$get()        # If no matrix found get data from input
        matrix <- solve(data, ...) # Calculate inverse 
        x$setinv(matrix)           # set value in cache space
        return(matrix)             # also return calculated value 

}


