## the following function makeCacheMatrix creates a cached matrix for faster computing.
## note, the cached matrix must remain unchange, else this function will need to run again.

makeCacheMatrix <- function(x = matrix()) {
	inv = NULL
	set = function(y) {
		x <<- y
		inv <<- NULL
	}
	get = function() x
	setinv = function(inverse) inv <<- inverse
	getinv = function() inv
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve checks whether a matrix is cached
## if a matrix is not cached, the function passes the values to makeCacheMatrix
## note, this variant attempts to also present the time spent caching the matrix
## if a matrix is cached, the function states the case before returning it

cacheSolve <- function(x, ...) {
        inv = x$getinv()
        
        if (!is.null(inv)){
        	start.time = Sys.time()
        	# attempts to establish a start time as the system time
        	message("caching matrix...")
        	# states the function is working
        	dur = Sys.time() - start.time
        	# creates an object denoting the duration of the process
        	message("matrix cached in [dur] seconds")
        	# returns the time it took to cache the matrix
        	return(inv)
        	# returns the newly cached matrix
        }
        message("returning cached matrix")
        # states the matrix was already cached
        mat.data = x$get()
        # retrieves get from makeCacheMatrix
        inv = solve(mat.data, ...)
        # creates an inverse function
        x$setinv(inv)
        # inverts the set object from makeCacheMatrix
        return(inv)
        # returns the inverse of the cached matrix
}
