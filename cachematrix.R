@@ -1,15 +1,35 @@
## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix specifies a matrix which will be stored at a global level.
## It contains a slove function to find the inverse of the matrix passed to it.
## cacheSolve checks for the presence of the inverse in global cache.  If the
## inverse was not cached, it uses the solve method in makeCacheMatrix to find it.

## Write a short comment describing this function
## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {


	solve <- function(y) 	{ ## create inverse
 		mInv <<- solve(y) ## place inverse in cache
	  					}
	return(mInv) 	##Return inverse, regardless of value stored.
				##This value will be NULL when run for the first time.
}

##*************** END OF FUNCTION

## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        	##check for presence of inverse
		m <- makeCacheMatrix(x)
        	if(is.null(m)) { 
			## if matrix not cached
                message("Matrix not cached!  Finding inverse...")
					}
                makeCacheMatrix$solve(matr) ##return inverse of original matrix
		else { 
			##output inverse
			print(m)
        		}

}
