#function sets and gets the value of the matrix and inverse matrix and creats list containing 
#it all
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x 
        setinv <- function(solve) i <<- solve 
        getinv <- function() i 
        list(set = set, get = get, setinv = setinv, getinv = getinv) 
}


#function inverts the matrix using makeCacheMatrix() function, checks if the inverse matrix already 
#been calculated, sets the new value of the matrix into cash and outputs the inverse matrix
cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) { 
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i) 
        i 
}
