## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setmatrix_inverse <- function(matrix_inverse) m <<- matrix_inverse
	getmatrix_inverse <- function() m
	list(set = set, get = get, setmatrix_inverse = setmatrix_inverse, getmatrix_in

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         ## Return a matrix that is the inverse of 'x'
	m <- x$getmatrix_inverse()
	if(!is.null(m)) {
		message("getting cached data!")
		return(m)
	}
	data <-x$get()
	m <- solve(data)
	x$setmatrix_inverse(m)
	m
}
