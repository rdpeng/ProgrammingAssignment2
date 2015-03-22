#pair of functions that cache the inverse of a matrix
#makeCacheMatrix creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(data_matrix=matrix()) {
        stored_inv <- NULL
        set <- function(y) {
                data_matrix <<- y
                stored_inv <<- NULL
        }
        get <- function() {
            return (data_matrix)
        }
        setinv <- function(sent_replacement_inv){
            stored_inv <<- sent_replacement_inv
        }
        getinv <- function() {
            return(stored_inv)
        }
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
#cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(madeMatrix, ...) {
        
        local_inv <- madeMatrix$getinv()
        
        if(!is.null(local_inv)) {
                message("getting cached data")
                return(local_inv)
        }
        else {
            local_data <- madeMatrix$get()
            local_inv <- solve(local_data, ...)
        
            madeMatrix$setinv(local_inv)
            return(local_inv) 
        }
}
