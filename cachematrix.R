## This function returns inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
m <- solve(x)
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
}


## This function returns the inverse of x

cacheSolve <- function(x=matrix()) {
        m <- solve(x)
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

}
