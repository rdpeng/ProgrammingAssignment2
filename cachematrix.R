## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

                  m <- NULL
        set <- function(y) 
		{
                x <<- y
                m <<- NULL
        }
        get <- function() {x}
        setmatrixinv <- function(solve) m <<-solve
        getmatrixinv <- function() {m}
        list(set = set, get = get,
             setmatrixinv = setmatrixinv,
             getmatrixinv = getmatrixinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
		
        mat <- x$get()
        m <- solve(mat)
        x$setmatrixinv(m)
        return(m)
		
}
