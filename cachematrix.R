## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a function that takes in a matrix. Four functions namely set, get, setinverse, getinverse have been defined inside the 
makeCacheMatrix function. set function sets the value of matrix. get function takes in the value of edited or non-edited matrix. setinverse sets the inverse a
getinverse gets the inverse of matrix. 


makeCacheMatrix <- function(x = matrix()) 
 
     {
        m <- NULL
        set <- function(y) 
                 {
                    x <<- y
                    m <<- NULL
                 }
        get <- function() 
                 {
                    x
                 }
        setinverse <- function(inverse) 
                        {
                          m <<- inverse
                        }
        getinverse <- function() 
                        {
                         inverse
                        }
        list(set = set, get = get,
         	 setinverse = setinverse,
             getinverse = getinverse)

     }


## cacheSolve takes in above function( a list basically) and computes the inverse of matrix. If matrix has been defined before, message("getting cached data") 
will display and the value of inverse would be returned from cache. If there is a new matrix, then the inverse would be calculated. However if the same function 
is used again, the inverse would be taken up from cache.   

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m

}
