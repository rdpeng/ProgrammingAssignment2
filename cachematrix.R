## Put comments here that give an overall description of what your
## functions do

## ===============
## makeCacheMatrix
## ===============
## return an object as a list of four functions (methods): 
## set(M)        - sets the state of the matrix to M
## get()         - gets the state of the matrix
## setinverse(I) - memorizes I as the inverse of the matrix
## getinverse()  - gets the inverse of the matrix memorized
## If an argument is provided, the matix is initialized accordingly
   
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv

	list(	set = set, 
		get = get, 
		setinverse = setinverse, 
		getinverse = getinverse
	)
}


## =========
## cachSolve
## ========= 
## Receives a CacheMatrix as an argument
## gets the inverse from cache if it exists
## otherwise solves and caches the inverse
## It returns the inverse in either case

cacheSolve <- function(x) {

	## First check and use cached value
	## otherwise solve and cache the inverse 

	inv <- x$getinverse()

	if (!is.null(inv)) {
		message("inverse already cached")
	} else {	
		dta <- x$get()
		inv <- solve(dta)
		x$setinverse(inv)
	}

	## return the inverse value in either case
 
	inv
}
