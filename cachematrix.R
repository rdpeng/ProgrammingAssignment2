## two functions 'makeCacheMatrix', 'cacheSolve' help reduce the burden of repeated calculation of matrix inverse 
## by storing the once calculated values into the cache which is available for further use, thus helping 
## in decreasing the amount of time and effort involved in inversion of the same matrix when needed.


## the function 'makeCacheMatrix' takes a  matrix as input and returns the inverse of that matrix and stores it in cache.
makeCacheMatrix <- function(x = matrix()) {
       
        inverse_matrix <- NULL ## Initialising and setting the variable 'inverse_matrix'nto 'NULL'
        
		get <- function() x  ## Display the contents of the matrix
		setinverse <- function(solve) inverse_matrix <<- solve ## The inverse of the matrix is calculated using the function 'solve()'
        getinverse <- function() inverse_matrix ##  The values of the 'inverse_matrix' are displayed using 'get'
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
       ## Inverse of the matrix 'x' is created and stored in the cache for further use

    	
 #######################################

## cacheSolve is a function to find the inverse of a matrix. In case, if inverse of the matrix has already been claculated, 
##it is looked for in the cache and returned
cachSolve <- function(x, ...) {
	## Return the inverse of the given matrix 'x'
        inverse_matrix <- x$getinverse()
        ## checking for the inverse matrix in the cache( if previously inverse was calculated)
        if(!is.null(inverse_matrix)) {
        	
                message("getting cached data") ## display this message if inverse of matrix is available in the cache
                return(inverse_matrix) ## return the value of inverse of matrix
        }
        ## The inverse of matrix is calculated if it is not already claculated and the value is not available in the cache
        data <- x$get()  ## elements of the matrix 'x' are read into 'data'
        inverse_matrix <- solve (data, ...) ## using function 'solve()' to carrying out the inversion of the matrix
        x$setInverse(inverse_matrix) 
        inverse_matrix ## inverse of matrix is stored in the variable 'inverse_matrix' is returned
}