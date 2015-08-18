## makeCacheMatrix specifies a matrix which will be stored at parent level.
## It contains a solve() function to find the inverse of the matrix passed to it.
## cacheSolve checks for the presence of the inverse in parent cache.  If the
## inverse was not cached, it uses the solve method in makeCacheMatrix to find it.

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  mInv<-matrix() ##initialize a local, empty inverse
  
  ##if inverse already exists, return a null matrix
  
  setInv<- function(z) {
    mInv<<- solve(z)
  }
  getInv<-function() mInV
  
	solve <- function(y) 	{ ## create inverse
 		mInv <<- solve(y) ## place inverse in cache
	  					}
	return(mInv) 	##Return inverse, regardless of value stored.
				##This value will be NULL when run for the first time.
}

##*************** END OF FUNCTION

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
 ##check for presence of inverse
		m <- makeCacheMatrix(x)
        	if(is.null(m)) { 
			## if matrix not cached
                message("Matrix not cached!  Finding inverse...")
					
                makeCacheMatrix$solve(matr) ##return inverse of original matrix
        	}else { 
			##output inverse
			print(m)
        		}

}
