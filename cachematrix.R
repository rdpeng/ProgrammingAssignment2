## Programming Assignment2
## In this program, two functions are provided to calculate
## inverse matrix and then cache the input and result in
## memory for query.
## Usage: source('cachematrix.R')
##        x <- makeCacheMatrix(mat)  # mat is an invertible matrix
##        cacheSolve(x)              # get the inverse result
## makeCacheMatrix: set and get invertible square matrix
##                  and its inverse result
## @param: invertible square matrix
## @return: function list, set/get for cached matrix,
##          setInv/getInv for cached result inverse matrix
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInv <- function(Inv) inv <<- Inv
	getInv <- function() inv

	list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## cacheSolve: get inverse matrix from cached result, if this
##             result is not available, calculate it and save
##             the answer in cache.
## @param: result from makeCacheMatrix
## @return: inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getInv()
		if (!is.null(inv)) {
			return(inv)
		}

		inv <- solve(x$get())
		x$setInv(inv)
		return(inv)
}
