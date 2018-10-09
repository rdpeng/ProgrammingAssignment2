## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {			##Default argument is matrix to cache the inverse
	i <- NULL								##Initialise inverse i to NULL					
	set <- function(y = matrix()) {				##Set function is defined to set the value of the matrix	 
                x <<- y
                i <<- NULL						##Inverse i of the matrix is set to NULL once more
        }
        get <- function() { x }					##Get function return the matrix itself
        setinv <- function(inverse) { i <<- inverse }  	##Setinv function sets the inverse that is calculated to inv
        getinv <- function() { i }					##Getinv function retrieves the inverse value
        list(set = set, get = get,					##Add the functions to a list
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {					##cacheSolve function takes a matrix x and gives its inverse
	
	  i <- x$getinv()							##Retrieve value of getinv from list
        if(!is.null(i)) {						##If there is a value in getinv(), it means the inverse has been cached
                message("getting cached data")
                return(i)						##Return the cached inverse
        }
        data <- x$get()							
        i <- solve(data, ...)						##If no value in getinv(), then calculate the inverse using solve() function
        x$setinv(i)							##Invoke setinv() function to set the inverse value in the list
        i
}
