## Put comments here that give an overall description of what your
## functions do

# 

## Write a short comment describing this function
  	# A list of functions to operate on our matrix
 	# First run this on the matrix you wish to invert.

makeCacheMatrix <- function(OrigMatrix = matrix()) {
	
	InvMatrix <<- NULL
	
	#Assigns a new input matrix. Resets inverted matrix to NULL
 	setOrigin <- function(new_matrix) {
		OrigMatrix <<- new_matrix
		InvMatrix <<- NULL
		}

	#Retrieves the input matrix
	getOrigin <- function() OrigMatrix
	
	#Caches the inverted matrix
	setInverse <- function(inverted_matrix) InvMatrix <<- inverted_matrix 
	
	#Retrieves the inverted matrix
	getInverse <- function() InvMatrix
	
	#Stick them in a list
	list(	setOrigin = setOrigin,
		getOrigin = getOrigin,
		setInverse = setInverse,
		getInverse = getInverse
		)

}


## Write a short comment describing this function

  	# Run this on the result from makeCacheMatrix. 
	# If the inverted matrix is cached, retrieve it
	# If it is not cached, calculate it and cache it.

cacheSolve <- function(CacheMatrix, ...) {
        ## Return a matrix that is the inverse of our previously cached matrix.

	if (is.null(CacheMatrix$getInverse()) ){			#If Inverse doesn't exist (yet)
		print("Generating Inverse")
		Inverse <- solve(CacheMatrix$getOrigin())		#Calculate it.
		CacheMatrix$setInverse(Inverse)		 	#Cache it.
		return(Inverse)						#Return it.		
		}
	else {								#If Inverse does exist
		print("Retrieving cached matrix")
		CacheMatrix$getInverse()				#Retrieve it. 
		}

}
