## The functions here creates a matrix and calculates its inverse. This inverse is then cached and stored 
## to avoid calculating it again. After that, the second function computes if the inverse of a given matrix is already
## stored in cache. If it is, the function will use the cached inverse. If it's not, it will calculate the inverse
## of the new matrix. hh

## Function makeCacheMatrix: its a function that stores a list of functions (set, get, setinverse and getinverse).
## You need to use list() to have the "subfunctions" you want inside your makeCacheMatrix function.
## set function creates a matrix and setinverse assigns the value of the matrix inverse to object m.
## get and and getinverse takes the matrix and its inverse respectively.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


        
## Function cacheSolve checks if the inverse of a given matrix is already calculated and stored in cache.
## If this is the case (if(!is.null(m)) condition), it will take the stored value, showing the message
## "getting cached data".
## If condition is not accomplished, it will calculate the new matrix inverse and will store it in cache (object m)  

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
