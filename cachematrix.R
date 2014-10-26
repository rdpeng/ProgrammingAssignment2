## The two functions below return a matrix that is the inverse of the matrix given in parameter.

## This first function assigns values of the different variables and create a list of functions
## that set the value of the matrix, get the value of the matrix, set the value of the result (inverted matrix)
## and get the value of the result.

makeCacheMatrix <- function(mat = matrix()){
	i <- NULL
	set <- function (y){
		mat <<- y
		i <<- NULL
	}
	get <- function() mat
	setinv <- function(inv) i <<- inv
	getinv <- function() i
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This second function inverts the matrix given as a parameter if it has already been calculated,
## otherwise it inverts the matrix and sets the value in memory. 

cacheSolve <- function(mat, ...) {
        i <- mat$getinv()
		if(!is.null(i)) {
            message("getting cached matrix")
            return(i)
        }
        data <- mat$get()
        i <- solve(data)%*%data
        mat$setinv(i)
        i
}
