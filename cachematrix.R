# makeCacheMatrix module
# create a special matrix that can cache results of time consuming calculations
# To save computing time, the calculation is done only if the matrix has changed

#
# makeCacheMatrix function
# 
# Holds a matrix and cache results of operations on that matrix
#
makeCacheMatrix <- function(x = matrix()) 
{	 
	#
	# internal cached matrix inverse value
	#
    inverseOfMatrix <- NULL	
	
	#
	# Internal flag telling us that the matrix has changed
	# NOTE: Its value starts at TRUE, so caller code will "know" 
	# that the inverse needs to be calculated
	#
	bMatrixHasChangedSinceLastSetInverseCall <- TRUE		

	#
	# Function to set the matrix
	#
    set <- function(y) 
	{				
		#
		# Verify if new matrix has differents sizes or differents values
		#	
		if(dim(x) != dim(y) || !all(x == y))
		{
			#
			# Raise our internal flag telling us that the matrix has changed, so the inverse needs to be calculated again
			#
			bMatrixHasChangedSinceLastSetInverseCall <<- TRUE				
		}

		#
		# Set the matrix
		#	
        x <<- y	
    }

	
	#
	# Function to retrieve the matrix
	#
    get <- function()
	{	
		x
	}
	
	
	#
	# Function to set the inverse of the matrix
	#
    setinverse <- function(inverse)
	{		

		#
		# Cache the inverse value
		#
		inverseOfMatrix <<- inverse		

		#
		# Reset our internal flag telling us that the matrix has changed, 
		# so the inverse won't be calculated again (until the matrix has changed again)
		#
		bMatrixHasChangedSinceLastSetInverseCall <<- FALSE
	}

	
	#
	# Function to retrieve the inverse of the matrix
	#
    getinverse <- function()
	{			
		inverseOfMatrix	
	}
	
	
	#
	# Function to retrieve the status of the matrix 
	# since the last time the  "SetInverse()" function has been called
	#
	matrixHasChangedSinceLastSetInverseCall <- function()
	{			
		bMatrixHasChangedSinceLastSetInverseCall	
	}	

  
	#
	# Returns the list of functions accessible externally
	#
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse, matrixHasChangedSinceLastSetInverseCall = matrixHasChangedSinceLastSetInverseCall)
			
}


#
# cacheSolve function
# 
# Calculate the inverse of the special matrix: "makeCacheMatrix"
#
cacheSolve <- function(x, ...) 
{
	
	#
	# Verify if matrix has changed since the last time this function has been called
	#
	if(x$matrixHasChangedSinceLastSetInverseCall() == FALSE)
	{
		#
		# message to user
		#
		message("Matrix hasn't changed, getting cached data")
		
		#
		# Return the inverse
		#
		return(x$getinverse())
	}

	#
	# Retrieve the matrix
	#
	data <- x$get()
	
	#
	# Calculate the inverse
	#
	inverseOfM <- solve(data, ...)

	#
	# message to user
	#
	message("Matrix has changed, getting calculated data")
    
	#
	# this call will refresh the cached inverse value,
	# reset the "matrixHasChangedSinceLastSetInverseCall" flag 
	# and return the inverse
	#
	x$setinverse(inverseOfM)    
	
}

