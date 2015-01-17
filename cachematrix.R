## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix sets/gets the value of a cached matrix, and sets/gets the value of its cached inverse
##          when the matrix is initially set, the corresponding inverse is reset to NULL
## if the inverse has been set (using cacheSolve), the inverse can be retreived from the cache - thus bypassing re-computation of the inverse.
## makeCacheMatrix returns a list - in this case, a list of the functions: setmat, getmat, setinv, getinv
##   makeCacheMatrix$getmat() is a function call returning the current cached matrix  (if it has been set)
##   makeCacheMatrix$getinv() is a function call returning the current cached inverse (i.e. if it has been computed & cached)
##       this method allows the matinv in this function to be a variable in the cached environment
##       so makeCacheMatrix$getinv gets the inverse matrix in the mCM environment
##       so that cacheSolve expects to be called with input where input$getmat() is the matrix that needs to be inverted

makeCacheMatrix <- function(x = matrix()) {
	matinv <- NULL
	setmat <- function(y) { 
		x      <<- y		# to cached matrix, x
		matinv <<- NULL		# to cached inverse, matinv: note that when you set a new cached "x", 
        }                               #  you reset (NULL) the inverse. 
        getmat <- function() x
        setinv <- function(inv) matinv <<- inv  
        getinv <- function()    matinv
        list (setmat = setmat, getmat = getmat, setinv = setinv, getinv = getinv) # last statement in the function 
}                                                                     # function call to makeCacheMatrix will return this list of functions 


## Write a short comment describing this function
#  cacheSolve is called with input where input$getmat() is the matrix that needs to be inverted
#        when cacheSolve returns, the ...$getinv() will contain the inverse,
#           so makeCacheMatrix$getinv gets the inverse matrix in the mCM environment

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinv()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
####    if you got here, you need to 
	data <- x$getmat()
	m <- solve(data, ...)
	x$setinv(m)
	m	
}
