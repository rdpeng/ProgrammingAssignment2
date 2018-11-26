##makeVector creates a special "vector", which is really a list containing a function to

makeVector <- function(x = numeric()) {
#set the value of the vector
#get the value of the vector
#set the value of the mean
#get the value of the mean
	m <- NULL
      set <- function(y) {
                x <<- y
                m <<- NULL
      }
      get <- function() x
      setmean <- function(mean) m <<- mean
      getmean <- function() m
      list(set = set, get = get,
      setmean = setmean,
      getmean = getmean)
}

##The following function calculates the mean of the special "vector" created with the above function
##However, it first checks to see if the mean has already been calculated
##If so, it gets the mean from the cache and skips the computation
##Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}


##  This function creates a special "matrix" object that can cache its inverse
## SetInverse will set value of the invertible matrix and 
## GetInverse will bring value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
#set matrix value	
	set <- function(y){
		x <<- y
		inv <<- NULL 
	}
	get <- function ()x
	setInverse <- function(inverse) inv <<-inverse
	getInverse <- function() inv
	list(set = set, get = get,
             setmean = setmean,
             getmean = getmean) 
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
       
 # Return a matrix that is the inverse of 'x'
 
	inv <- x$getInverse()
	if(!is.null(inv)) {
		message("Getting cached invertible matrix")
		return(inv)
	}
	Matrix <x$get()
	inv <- solve(Matrix)
	x$setInverse(inv)
	inv
}
