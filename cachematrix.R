## Second assignment for R class.
## This has 2 functions.   
##    makeCacheMatrix.   Hold in an enviroment the inverse of
##                       a matrix.  
##    cacheSolve         Uses makeCacheMatrix has a data store.  If
##                       the matrix has been previously inverted then
##                       this will return the inverted copy of the
##                       matrix.


makeCacheMatrix <- function (x = matrix() ) {
#
#             functions 
#                    setvector     initialize condition
#                    getvector     return vector (not used)
#                    getinverse    return inverted matrix 
#                    setinverse    set the inverted matrix
#
	hold_inv <- NULL
	setvector <- function(input1) {
		x <<- input1
		hold_inv    <<- NULL
	}
	getvector <- function() { x  }
	setinverse   <- function(inv_matrix)  { hold_inv <<- inv_matrix }
	getinverse   <- function() { hold_inv }
	list( setvector    = setvector, 
		getvector    = getvector, 
            getinverse  = getinverse,
            setinverse   = setinverse )

   
}

cacheSolve <- function(x, ...) {
	#    check to see if I have inverted this matrix or not.  It
	#    will be null if this is the first time

	m <- x$getinverse()
	if(is.null(m)) {
		print ( "new guy" )
		work_matrix <- x$getvector()
		temp_inv = solve(work_matrix)
		m <- x$setinverse(temp_inv)
      } else {
		print ( "cache" )
      }
	m
}
#    test cases
#    a <- matrix(c(4,3,3,2),2,2)
#    zippy <- makeCacheMatrix(a)
#    b <- cacheSolve(zippy)
