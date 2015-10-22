# Coursera - R Programming
# Assignment 2
# David Peerson

# For this assignment, assume that the matrix supplied is always invertible.


# ------------------------------------------------------------------------------
# The folowing function creates a cache of input data, as well as sub-functions
# to get and set the their values
#
makeCacheMatrix <- function(inpData = matrix()) {
	# Instantiate an internal variable for holding the cached matrix
	fMatrix <- NULL
	
	# Function to (re)set the input data
	set <- function(inpNewData) {
		inpData <<- inpNewData 		# Cache the new input, so that it can then be calc'd in a later step
		fMatrix <<- NULL 				# Cache the variable holding the computationally intensive result
	}
	
	# Function to get the input data
	get <- function() inpData
	
	# Function to (re)set the cached computed result
	setMatrix <- function(inpNewMatrix) fMatrix <<- inpNewMatrix
	
	# Function to get the cached computed result
	getMatrix <- function() fMatrix
	
	# "Expose" a list of functions for getting and setting the cached data
	list(set = set, get = get,
		  setMatrix = setMatrix,
		  getMatrix = getMatrix)
}



# ------------------------------------------------------------------------------
# The folowing function returns a matrix that is the inverse of 'x', preferably 
# from its cache.
#
cacheSolve <- function(inpMatrix, ...) {
	# Instantiate a working variable and get the cached matrix
	fMatrix <- inpMatrix$getMatrix()
	
	# Test whether the calculation has already been performed
	if(!is.null(fMatrix)) {
		message("getting cached data")
	} else {
		# Get the source input data
		fData <- inpMatrix$get()
		
		# Make sure that the input data is in fact a matrix
		if(is.matrix(fData)){
			# Compute the new (inverse) matrix, and store it in the cache
			fMatrix <- solve(fData, ...)
			inpMatrix$setMatrix(fMatrix)
		} else {
			# Tell the user we can't solve the input data
			message("unable to 'solve()' input data:")
			# Send back the non-matrix data that the user supplied (GIGO)
			return(fData)
		}
	}
	# Return the cached matrix
	return (fMatrix)
}
