## This functions cache the square matrix and returns the inverted Square matrix 
## MakeCache Matrix function takes the inputs for the matrix and call function to invert the matrix data set. 
## It creates special matrix Object which cache its inverted square Matrix 
## This function returns 4 elements in the list as get, set, getinver and setinver. These functions passed to CacheSolve function to calculate the inverted square matrix

makeCacheMatrix <- function(x = matrix()) {
		# Input the Square matrix to the function MakeCacheMatrix
        m = NULL
        set = function(y) {			# y is used as dummy matrix to pass value from Get function to Set function. 
                x <<- y        		# Create x which is reterieved from get()  
                m <<- NULL			# creating Variable m with NULL matrix values
        }
        get = function() x
        setinver <- function(inver1){ 
			m <<- inver1		# Setinver function assigns m variable value retrieved from inverted square Matrix inver1
		}
        getinver <- function(){ 
			m
		}
        list(set = set, get = get,
             setinver = setinver,
             getinver = getinver)
}


## CacheSolve function returns the inverted the square matrix input from the MakeCacheMatrix function.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        #Inverting the square matrix 
        m <- x$getinver()
		# Check if Inverse has already been calculated
        if(!is.null(m)) {
                message("getting cached data")
				## Return the cache matrix as inverted square matrix 
                return(m)
        }
		# Assign the square matrix to "data" object
        data <- x$get()
		# calculate the inverse of square matrix using Solve R Method
        m <- solve(data, ...)
        # Set the inverted matrix in cache through setinver function
		x$setinver(m)
		# Return the inverted Matrix
        return(m)
}