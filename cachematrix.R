
#Matrix inversion is usually a costly computation and there may be some benefit
#to caching the inverse of a matrix rather than compute it repeatedly

#The first function, makeCacheMatrix creates a special "Matrix", which is really a 
#list containing a function to:
#1 set the values of the Matrix
#2 get the values of the Matrix
#3 set the value of the Inverse
#4 get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
	
	      I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setInv <- function(Inv) I <<- Inv
        getInv <- function() I
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


#The following function calculates the Inverse of the special "Matrix" 
#created with the above function. However, it first checks to see if the Inverse
#has already been calculated. If so, it gets the Inverse
#from the cache and skips the computation. Otherwise, it calculates and set the Inverse 
#in the cache via the setInv function.

cacheSolve <- function(x, ...) {
	
	   I <- x$getInv()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        
        matrix <- x$get()
        I <- solve(matrix, ...)
        x$setInv(I)
        
        I
	
        ## Return a matrix that is the inverse of 'x'
}
