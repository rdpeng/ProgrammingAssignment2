## For the reason that matrix inversion is a work that often consumes a lot of time
## We design these two functions below to get the cached value of the inversion,
## or if the value is not computed yet, just calculate it and cache it

# The first function, makeChcheMatrix creates a special "matrix", which is actually a list containing a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
	mat.inv <- NULL
	set <- function(y) {
		x <<- y
		mat.inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) mat.inv <<- inverse
	getinverse <- function() mat.inv
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


#The following function calculates the inverse of the special "matrix" created with the above function. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
	mat.inv <- x$getinverse()
	if(!is.null(mat.inv)){
		message("getting cached data.")
		return(mat.inv)
	}
	data <- x$get()
	mat.inv <- solve(data)
	x$setinverse(mat.inv)
	mat.inv
}


##Sample Run:
##>  x = matrix(rnorm(16,10,5), nrow = 4)
##> x
##[,1]      [,2]      [,3]      [,4]
##[1,] 13.945253 11.954432  8.325649  7.650011
##[2,]  7.821810  9.869586 10.315409 16.809106
##[3,]  6.117695  1.888886 18.763949 12.859572
##[4,] 12.396258  9.558279 12.602538  3.551862
##>  x1 = matrix(rnorm(16,10,5), nrow = 4)
##> x1
##[,1]      [,2]      [,3]      [,4]
##[1,] 4.635362 10.052303  8.768874  5.737718
##[2,] 7.990009 14.907989  4.508591 11.517577
##[3,] 6.010579  8.424601 10.219754  6.142770
##[4,] 3.480303 15.760099 10.410194 15.244832
##> m1 = makeCacheMatrix(x1)
##> cacheSolve(m1)  #cachesolve for the first time
##[,1]        [,2]        [,3]        [,4]
##[1,] -0.19767773  0.11654956  0.22407669 -0.10394349
##[2,]  0.37702268  0.01829079 -0.29335171 -0.03751589
##[3,]  0.02139719 -0.09543893  0.09558038  0.02553822
##[4,] -0.35924867  0.01965554  0.18684280  0.11067037
##> cacheSolve(m1) #cachesolve for the second time, directly get the cached value
##getting cached data.
##[,1]        [,2]        [,3]        [,4]
##[1,] -0.19767773  0.11654956  0.22407669 -0.10394349
##[2,]  0.37702268  0.01829079 -0.29335171 -0.03751589
##[3,]  0.02139719 -0.09543893  0.09558038  0.02553822
##[4,] -0.35924867  0.01965554  0.18684280  0.11067037

