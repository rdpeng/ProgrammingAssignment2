## Put comments here that give an overall description of what your
## functions do
#First function create a list of functions to set, get a matrix and cache its inverse for next usage
#Second function calculate the inverse of matrix object returned from first function. However, in case of inverse has already calculated, it will retrieve it from cache

## Write a short comment describing this function
#This function create a matrix object. 
#Function 'set' is used to assign the value of matrix to variable x. 
#Whereas the x is a free variable (not argument of 'set' function), 
#it is impossible to assign a value to it by <- sign because it will create a local variable.
#So the <<- sign was used to overcome this problem. 
#Function 'get' is used to return the value of matrix
#Function 'setinv' is used to assign the inverse of matrix to free variable inv (as described above).
#Function 'getinv' is used to return the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	list(
		set = function(y){
		x <<- y
		inv <<- NULL
		},
		get = function(){
			x
		},
		setinv = function(inverse){
			inv <<- inverse
		},
		getinv = function(){
			inv 
		})
}


## Write a short comment describing this function
#This function gets the matrix(x) function created by 'makeCacheMatrix'.
#It first checks that inverse of matrix has already calculated.
#If inverse of matrix is find, so return inverted matrix
#If inverse of matrix is NULL, the function will get the matrix and calculate the inverse of it by 'solve' function
#The inverted matrix sets in 'setinv' for next use.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	inv  <- solve(x$get(), ...)
	x$setinv(inv)
	inv
}
