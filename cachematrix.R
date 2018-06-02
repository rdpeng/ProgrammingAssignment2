
###################################################### 
## Functions cache inverse matrix to save on costly ## 
## computations and repetitive processing		    ##
## ###################################################


## This function creates a special "matrix" object that can cache its inverse.					   

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated then cacheSolve should retrieve the inverse from the cache and original matrix

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
               
                message("getting cached data")
               	return(list("original_matrix"=x$get(), "inverse_matrix"= m))
                
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        
        list("original_matrix" = data, "inverse_matrix" = m)
}
