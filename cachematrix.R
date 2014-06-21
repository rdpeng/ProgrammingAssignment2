makeCacheMatrix <- function(x = matrix ()) {
  			m <- NULL
  			set <- function(y) {
    		x <<- y
    		m <<- NULL
  		} 
  
  ## this funtion creates a special object labeled "matrix" that can cache its inverse
  
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}
 ## this function calculates the inverse of the above "matrix" retuned by makeCacheMatrix
 
cachesolve <- function(x=matrix(), ...) {
  			m <- x$getmatrix()
  			if(!is.null(m)) {
    		message("getting cached data")
    		return(m)
  		}
 
 ## If the inverse has already been calculated
 ## cachesolve will retrieve the inverse from the cache
 
 data <- x$get()
  		m <- solve(data, ...)
  		x$setmatrix(m)
  		m
 
}
