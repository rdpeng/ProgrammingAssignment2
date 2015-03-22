## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix(x) function is using for store the inverse matrix;
## cacheSolve function is using for computing the inserve matrix . if the inverse matrix was exsited in the store matrix it will skip. compute.
##

## Write a short comment describing this function
## makeCacheMatrix(x) do the following things:
## 1: set the matrix ; 2: get the matrix ; 3: set the inverse matrix for the matrix x ; 4: fet the inverse matrix.


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
## cacheSolve function is using for computing the inserve matrix . 
## if the inverse matrix was exsited in the store matrix, it will skip the computing..


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
