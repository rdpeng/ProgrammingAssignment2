## There are totally two function 1. makeCacheMatrix(for caching inverse Matrix) 2.cacheSolve(which solves the matrix)


## makeCacheMatrix will create an empty matrix to store the calculated value and also gives function like 
##get,set,getMatrixCache data and setMatrixCache data which will be usefull to manipulate the cache data

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setMatrixCache <- function(mat) m <<- mat
        getMatrixCache <- function() m
        list(set = set, get = get,
             setMatrixCache = setMatrixCache,
             getMatrixCache = getMatrixCache)

}


## cacheSolve will calculate the inverse of a matrix and returns the inverse matrix at the end if it was calcuated before 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         m <- x$getMatrixCache()
        if(!is.null(m)) {
                message("getting cached data")
          			print("check")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setMatrixCache(m)
        m
}
