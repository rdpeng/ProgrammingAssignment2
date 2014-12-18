## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL #  m will be our 'matrix' and it's reset to NULL every
			#    time CacheMatrix is called
        set <- function(y) { # takes an input matrix
                x <<- y		# saves the input matrix 
                m <<- NULL	# resets the inverse matrix to NULL, basically what happens when a new object is generated.
        }
        get <- function() x # this function returns the value of the original matrix
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m # this will return the cached value to cacheSolve() on
					#  subsequent accesses
        list(set = set, get = get, #  OK, this is accessed each time makeCacheMatrix() is called,    
             setsolve = setsolve,  #   that is, each time we make a new object.  This is a list of 
             getsolve = getsolve)	#   the internal functions ('methods') so a calling function
						#   knows how to access those methods.    
}


cacheSolve<- function(x, ...) { # the input x is an object created by makeVector
        m <- x$getsolve()
        if(!is.null(m)) { # if solve was already cached (not NULL) ...
                message("getting cached data") # ... send this message to the console
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m) # store the calculated inverse matrix in x (see setsolve() in makeCacheMatrix
        m
}

