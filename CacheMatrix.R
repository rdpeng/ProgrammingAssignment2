## The function makeCacheMatrix creates a matrix for compute the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inmtx <- NULL
        ## Set the value of the matrix
        set <- function(y) {
                x <<- y
                inmtx <<- NULL
        }
        ## Get the value of the matrix
        get <- function() x
        ## Set the value of the matrix
        setinmtx <- function(inverse) inmtx <<- inverse
        ## Get the value of the matrix
        getinmtx <- function() inmtx
        list(set = set, get = get,
             setinmtx = setinmtx,
             getinmtx = getinmtx)
}



##The second function check the data, if the data already exists, it wont compute it, else it will compute it
cacheinverse <- function(x, ...) {
        inmtx <- x$getinmtx()
        if(!is.null(inmtx)) {
                message("getting cached data")
                return(inmtx)
        }
        data <- x$get()
        inmtx <- solve(data, ...)
        x$setinmtx(inmtx)
        inmtx
}
